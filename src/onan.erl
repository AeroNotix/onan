-module(onan).
-compile(export_all).


parse_vsn(Vsn) ->
    try
        mouture:parse(Vsn)
    catch
        error:function_clause ->
            {error, invalid_vsn}
    end.

to_dep_list([]) ->
    [];
to_dep_list(Dependencies) ->
    to_dep_list(Dependencies, []).

to_dep_list([], Converted) ->
    Converted;
to_dep_list([{Namespace, Name, Vsn}|T], Acc) ->
    JSONDep = [{<<"namespace">>, Namespace},
               {<<"name">>, Name},
               {<<"version">>, list_to_binary(Vsn)}],
    to_dep_list(T, [JSONDep|Acc]).

create_local_paths([]) ->
    [];
create_local_paths(Deps) when is_list(Deps) ->
    case os:getenv("HOME") of
        false ->
            error(this_shouldnt_happen);
        Home when is_list(Home) ->
            create_local_paths(Home, Deps)
    end.

create_local_paths(Home, Deps) ->
    [{DepName, DepVsn, onan_file:join_paths(Home, [".onan", DepName, DepVsn])}
     || {DepName, DepVsn} <- Deps].

do_deploy(Endpoint, Metadata) ->
    URL = Endpoint ++ "/artefact",
    AppJSON = "application/json",
    Headers = ["accept", AppJSON],
    Request = {URL, Headers, AppJSON, jsx:encode(Metadata)},
    Response = httpc:request(post, Request, [], []),
    {ok, {{_HTTP, Status, _Msg}, RespHeaders, Resp}} = Response,
    case Status of
        200 ->
            {ok, proplists:get_value("location", RespHeaders)};
        404 ->
            {error, {not_found, Resp}};
        409 ->
            {error, conflict_detected};
        422 ->
            {error, checksum_failure};
        _ ->
            {error, {unknown_error, Status}}
    end.

deploy({config, _, Config, _, _, _, _}, AppFile) ->
    %% The Dir passed to this handler seems to always be incorrect,
    %% rebar internally sets the cwd to the correct directory.
    {ok, Dir} = file:get_cwd(),

    OnanDeps     = proplists:get_value(onan_deps, Config, []),
    OnanEndpoint = proplists:get_value(onan_endpoint, Config),

    {ok, [{application, AppName, AppFileContents}]}
        = file:consult(AppFile),
    Vsn = list_to_binary(proplists:get_value(vsn, AppFileContents, "")),
    Description = list_to_binary(proplists:get_value(description, AppFileContents, "")),
    Namespace = list_to_binary(proplists:get_value(namespace, Config)),
    case parse_vsn(Vsn) of
        {error, invalid_vsn} ->
            %% We use semver, because reasons.
            io:format("The version supplied is invalid. "
                      "Semantic versioning is required. "
                      "Please see: http://semver.org/");
        _ ->
            %% TODO: Make this better.
            {ok, {_, ZipBytes}} = zip:create("",
                                             ["../" ++ filename:basename(Dir)],
                                             [{compress, all},
                                              memory,
                                              {uncompress, [".beam", ".app"]}]),
            Payload = base64:encode(ZipBytes),

            %% TODO: Make this better.
            %% Create the human-readable md5 of the payload.
            << M: 128>> = crypto:hash(md5, Payload),
            Checksum = list_to_binary(integer_to_list(M, 16)),

            DeploymentMetadata =
                [{<<"namespace">>, Namespace},
                 {<<"name">>, AppName},
                 {<<"version">>, Vsn},
                 {<<"dependencies">>, to_dep_list(OnanDeps)},
                 {<<"description">>, Description},
                 {<<"checksum">>, Checksum},
                 {<<"payload">>, Payload}],
            case do_deploy(OnanEndpoint, DeploymentMetadata) of
                {ok, Location} ->
                    io:format("Successfully deployed ~s. Artefact now "
                              "permanently lives at ~s.~n", [AppName,
                                                             OnanEndpoint ++ Location]),
                    ok;
                {error, {not_found, Extra}} ->
                    Error = proplists:get_value(<<"error">>,
                                                jsx:decode(
                                                  erlang:list_to_binary(Extra)
                                                 )),
                    io:format("Not found: ~p~n", [Error]),
                    {error, not_found};
                {error, checksum_mismatch} ->
                    io:format("The server received a different "
                              "checksum than what was calculated "
                              "prior to sending. Try again, perh"
                              "aps under a more secure connection~n"),
                    ok;
                {error, conflict_detected} ->
                    io:format("An artefact already exists with this "
                              "metadata or an attempt to create a "
                              "lower-versioned artefact was made.~n~n"),
                    {error, conflict_detected}
            end
    end.

package_project(Dir) ->
    {ok, {_, ZipBytes}} = zip:create("",
                                     ["../" ++ filename:basename(Dir)],
                                     [{compress, all},
                                      memory,
                                      {uncompress, [".beam", ".app"]}]),
    ZipBytes.

save_project(ProjPkg, Dir, Vsn) ->
    FinalPath = filename:join(Dir, Vsn),
    OutPkg = filename:join(FinalPath, "code.zip"),
    case filelib:is_file(OutPkg) of
        true ->
            io:format("ERROR: Project already exists on filesystem "
                      "refusing to overwrite");
        false ->
            ok = filelib:ensure_dir(OutPkg),
            ok = file:write_file(OutPkg, ProjPkg)
    end.

main(["install"]) ->
    {ok, Config} = file:consult("onan.config"),
    {ok, Dir} = file:get_cwd(),
    ProjName = proplists:get_value(name, Config),
    Vsn = proplists:get_value(vsn, Config),
    case os:getenv("HOME") of
        HomeDir when is_list(HomeDir) ->
            OnanHome = filename:join(HomeDir, ".onan"),
            ProjDir = filename:join(OnanHome, ProjName),
            ok = filelib:ensure_dir(ProjDir),
            ProjPkg = package_project(Dir),
            ok = save_project(ProjPkg, ProjDir, Vsn)

    end;

main(["deps"]) ->
    {ok, Config} = file:consult("onan.config"),
    {ok, Dir} = file:get_cwd(),
    Deps = proplists:get_value(deps, Config),
    LocalPaths = create_local_paths(Deps),
    io:format("~p~n", [LocalPaths]).
