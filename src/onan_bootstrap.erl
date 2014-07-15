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
bootstrap([Dir|Dirs]) ->
    ok = file:set_cwd(Dir),
    case file:consult("onan.config") of
        {ok, Config} ->
            %% TODO Move the deploy shit into its own proper namespace
            onan:deploy(Config);
        {error, enoent} ->
            bootstrap_from_app_src()
    end,
    bootstrap(Dirs).

bootstrap_from_app_src() ->
    FromSrc = filelib:wildcard("src/**/*.app.src"),
    FromBeam = filelib:wildcard("beam/**/*.app.src"),
    AppSrc =
        case {FromSrc, FromBeam} of
            {[], [A]} ->
                A;
            {[A], []} ->
                A
        end,
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
            onan:deploy(OnanConfig);
        {error, _} = E ->
            io:format("~p~n", [E])
    end.
