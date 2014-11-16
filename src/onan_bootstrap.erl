-module(onan_bootstrap).

-export([bootstrap/1]).


git_namespace() ->
    case filelib:is_dir(".git") of
        true ->
            case os:cmd("git remote -v") of
                "origin\tgit@github.com:" ++ Rest ->
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
    {Dir,
     proplists:get_value(namespace , Config),
     proplists:get_value(name      , Config),
     proplists:get_value(vsn       , Config)}.

deps_from_rebar(Dir) ->
    case file:consult(filename:join(Dir, "rebar.config")) of
        {error, enoent} ->
            [];
        {ok, []} ->
            [];
        {ok, [{deps, Deps}]} ->
            [element(1, Dep) || Dep <- Deps]
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
            [{application, Name, Attrs}] = AppSrcContents,
            ListName = atom_to_list(Name),
            Namespace = what_namespace(ListName),
            Vsn = proplists:get_value(vsn, Attrs),
            Desc = proplists:get_value(description, Attrs),
            OnanConfig =
                [{namespace, Namespace},
                 {name, ListName},
                 {vsn, Vsn},
                 {description, Desc},
                 {deps, []},
                 {server, "http://localhost:45045"}],
            Format = "~p.~n~p.~n~p.~n~p.~n~p.~n~p.~n",
            Output = io_lib:format(Format, OnanConfig),
            file:write_file("onan.config", Output),
            Deps = deps_from_rebar(Dir),
            {Dir, Namespace, Name, Vsn, Deps};
        {error, _} = E ->
            io:format("~p~n", [E])
    end.
