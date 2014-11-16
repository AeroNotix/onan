-module(onan_bootstrap).

-export([bootstrap/1]).


-type version() :: string().

-record(dependency,
        {dir :: string(),
         name :: string(),
         namespace :: string(),
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

bootstrap([]) ->
    ok;
bootstrap(Dirs) ->
    bootstrap(Dirs, []).

bootstrap([], Acc) ->
    io:format("~p~n", [Acc]),
    ok;
bootstrap([Dir|Dirs], Acc) ->
    ok = file:set_cwd(Dir),
    case file:consult("onan.config") of
        {ok, Config} ->
            bootstrap(Dirs, [info_from_config(Dir, Config)|Acc]);
        {error, enoent} ->
            bootstrap(Dirs, [bootstrap_from_app_src(Dir)|Acc])
    end.

info_from_config(Dir, Config) ->
    #dependency{
       dir=Dir,
       namespace=proplists:get_value(namespace, Config),
       name=proplists:get_value(name, Config),
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

bootstrap_from_app_src(Dir) ->
    FromSrc = filelib:wildcard("src/**/*.app.src"),
    FromEbin = filelib:wildcard("ebin/**/*.app.src"),
    FromApp = filelib:wildcard("ebin/**/*.app"),
    Paths = [FromApp, FromSrc, FromEbin],
    FindMetadataFile =
        fun([]) -> false;
           ([P]) -> {true, P}
        end,
    AppSrc = hd(lists:filtermap(FindMetadataFile, Paths)),
    case file:consult(AppSrc) of
        {ok, AppSrcContents} ->
    case lists:filtermap(FindMetadataFile, Paths) of
        [] ->
            io:format("Not an Erlang/OTP dependency: ~p~n", [Dir]);
            [{application, Name, Attrs}] = AppSrcContents,
            ListName = atom_to_list(Name),
            Namespace = what_namespace(ListName),
            Vsn = get_version(Dir, Attrs),
            Desc = proplists:get_value(description, Attrs),
            Deps = deps_from_rebar(Dir),
            OnanConfig =
                [{namespace, Namespace},
                 {name, ListName},
                 {vsn, Vsn},
                 {description, Desc},
                 {deps, Deps},
                 {server, "http://localhost:45045"}],
            Format = "~p.~n~p.~n~p.~n~p.~n~p.~n~p.~n",
            Output = io_lib:format(Format, OnanConfig),
            file:write_file("onan.config", Output),
            #dependency{dir=Dir,
                        namespace=Namespace,
                        name=Name,
                        version=Vsn,
                        deps=Deps}
    end.
