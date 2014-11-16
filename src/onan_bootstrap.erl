-module(onan_bootstrap).

-export([bootstrap/1]).


-type version() :: string().

-record(dependency,
        {dir :: string(),
         name :: string(),
         namespace :: string(),
         description :: string(),
         version :: version(),
         deps = [] :: [dependency()]}).

-type dependency() :: #dependency{}.


git_namespace() ->
    case filelib:is_dir(".git") of
        true ->
            case os:cmd("git remote -v") of
                "origin\tgit@github.com:" ++ Rest ->
                    [Namespace | _] = string:tokens(Rest, "/"),
                    {ok, Namespace};
                "origin\thttps://github.com/" ++ Rest ->
                    [Namespace | _] = string:tokens(Rest, "/"),
                    {ok, Namespace};
                "origin\thttp://github.com/" ++ Rest ->
                    [Namespace | _] = string:tokens(Rest, "/"),
                    {ok, Namespace};
                "origin\tgit://github.com/" ++ Rest ->
                    [Namespace | _] = string:tokens(Rest, "/"),
                    {ok, Namespace};
                _ ->
                    undefined
            end;
        false ->
            undefined
    end.

what_namespace(Name) ->
    case git_namespace() of
        {ok, Namespace} ->
            Namespace;
        undefined ->
            Input = io:get_line("Which namespace should be used for " ++ Name ++ "? => "),
            "\n" ++ Namespace = lists:reverse(Input),
            lists:reverse(Namespace)
    end.

yank_dependency(Name, Dependencies) ->
    case [D || D <- Dependencies, D#dependency.name == Name] of
        [] ->
            {error, missing_transitive_dependency};
        [Transitive] when is_record(Transitive, dependency) ->
            {ok, Transitive}
    end.

build_transitives([], _Dependencies, Acc) ->
    Acc;
build_transitives([#dependency{deps=[]} = Dep|Rest],
                  Dependencies, Acc) ->
    build_transitives(Rest, Dependencies, [Dep|Acc]);
build_transitives([#dependency{name=Name, deps=Deps} = Dep|Rest],
                  Dependencies, Acc) ->
    FilterTransitives =
        fun(DepName) ->
                case yank_dependency(DepName, Dependencies) of
                    {ok, #dependency{name=N, namespace=NS, version=V}} ->
                        {true, {N, NS, V}};
                    {error, missing_transitive_dependency} ->
                        io:format("Missing transitive dependency for ~s of ~s~n",
                                  [Name, DepName]),
                        false
                end
        end,
    TransitiveDeps = lists:filtermap(FilterTransitives, Deps),
    build_transitives(Rest, Dependencies,
                      [Dep#dependency{deps=TransitiveDeps}|Acc]).

build_transitives(Dependencies) ->
    build_transitives(Dependencies, Dependencies, []).

bootstrap([]) ->
    ok;
bootstrap(Dirs) ->
    bootstrap(Dirs, []).

bootstrap([], Dependencies) ->
    WithTransitives = build_transitives(Dependencies),
    [write_onan_config(Dependency, "https://localhost:45045")
     || Dependency <- WithTransitives],
    ok;
bootstrap([Dir|Dirs], Acc) ->
    ok = file:set_cwd(Dir),
    case file:consult("onan.config") of
        {ok, Config} ->
            bootstrap(Dirs, [info_from_config(Dir, Config)|Acc]);
        {error, enoent} ->
            case bootstrap_from_app_src(Dir) of
                {ok, Dependency} when is_record(Dependency, dependency) ->
                    bootstrap(Dirs, [Dependency|Acc]);
                {error, not_a_dependency} ->
                    bootstrap(Dirs, Acc)
            end
    end.

info_from_config(Dir, Config) ->
    #dependency{
       dir=Dir,
       namespace=atom_to_list(proplists:get_value(namespace, Config)),
       name=atom_to_list(proplists:get_value(name, Config)),
       version=proplists:get_value(vsn, Config)}.

deps_from_rebar(Dir) ->
    case file:consult(filename:join(Dir, "rebar.config")) of
        {error, enoent} ->
            [];
        {ok, []} ->
            [];
        {ok, Config} when is_list(Config) ->
            case proplists:get_value(deps, Config) of
                undefined ->
                    [];
                Deps when is_list(Deps) ->
                    [element(1, Dep) || Dep <- Deps]
            end
    end.

get_git_version(Dir) ->
    Cmd = "cd " ++ Dir ++ "; git describe --always --tags",
    os:cmd(Cmd).

get_version(Dir, Attrs) ->
    case proplists:get_value(vsn, Attrs) of
        git ->
            {ok, Vsn} = get_git_version(Dir),
            Vsn;
        Vsn when is_list(Vsn) ->
            Vsn
    end.

write_onan_config(#dependency{dir=Dir,
                              namespace=Namespace,
                              name=ListName,
                              version=Vsn,
                              description=Desc,
                              deps=Deps}, Server) ->
    OnanConfig =
        [{namespace, Namespace},
         {name, ListName},
         {vsn, Vsn},
         {description, Desc},
         {deps, Deps},
         {server, Server}],
    Format = "~p.~n~p.~n~p.~n~p.~n~p.~n~p.~n",
    Output = io_lib:format(Format, OnanConfig),
    file:write_file(Dir ++ "/onan.config", Output).

bootstrap_from_app_src(Dir) ->
    FromSrc = filelib:wildcard("src/**/*.app.src"),
    FromEbin = filelib:wildcard("ebin/**/*.app.src"),
    FromApp = filelib:wildcard("ebin/**/*.app"),
    Paths = [FromApp, FromSrc, FromEbin],
    FindMetadataFile =
        fun([]) -> false;
           ([P]) -> {true, P}
        end,
    case lists:filtermap(FindMetadataFile, Paths) of
        [] ->
            io:format("Not an Erlang/OTP dependency: ~p~n", [Dir]),
            {error, not_a_dependency};        
        [AppSrc|_] ->
            {ok, AppSrcContents} = file:consult(AppSrc),
            [{application, Name, Attrs}] = AppSrcContents,
            ListName = atom_to_list(Name),
            Namespace = what_namespace(ListName),
            Vsn = get_version(Dir, Attrs),
            Desc = proplists:get_value(description, Attrs),
            Deps = deps_from_rebar(Dir),
            {ok, #dependency{dir=Dir,
                             namespace=Namespace,
                             description=Desc,
                             name=ListName,
                             version=Vsn,
                             deps=Deps}}
    end.
