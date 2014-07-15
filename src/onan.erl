-module(onan).
-compile(export_all).

-on_load(init/0).


init() ->
    inets:start(),
    ssl:start().

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
    JSONDep = [{<<"namespace">> , list_to_binary(Namespace)},
               {<<"name">>      , list_to_binary(Name)},
               {<<"version">>   , list_to_binary(Vsn)}],
    to_dep_list(T, [JSONDep|Acc]).

copy_dep(DepName, FromDep, To) ->
    DepCode = filename:join(FromDep, "code.zip"),
    OutDep = onan_file:join_paths(To, ["deps", DepName]),
    ok = filelib:ensure_dir(OutDep),
    zip:unzip(DepCode, [{cwd, OutDep}]).

save_remote_deps([]) ->
    ok;
save_remote_deps([{Namespace, Name, Vsn, Payload}|T]) ->
    Target = home_repo(Namespace, Name),
    Decoded = base64:decode(Payload),
    save_project(Decoded, Target, Vsn),
    save_remote_deps(T).

save_remote_deps_to_project([]) ->
    ok;
save_remote_deps_to_project([{Namespace, Name, Vsn, _}|T]) ->
    {ok, CWD} = file:get_cwd(),
    Target = filename:join(home_repo(Namespace, Name), Vsn),
    copy_dep(Name, Target, CWD),
    save_remote_deps_to_project(T).

get_remote_dependency(Namespace, Name, Vsn, Config) ->
    URI = proplists:get_value(server, Config),
    FullURI = lists:flatten(io_lib:format("~s/deps/~s/~s/~s", [URI, Namespace, Name, Vsn])),
    Req = {FullURI, [{"accept", "application/json"}]},
    {ok, Resp} = httpc:request(get, Req, [], []),
    {{_, Status, _}, _, Body} = Resp,
    case Status of
        404 ->
            {error, notfound};
        200 ->
            {ok, list_to_binary(Body)}
    end.

extract_dependency_data(Dep) ->
    Namespace = proplists:get_value(<<"namespace">>, Dep),
    Name      = proplists:get_value(<<"name">>     , Dep),
    Version   = proplists:get_value(<<"version">>  , Dep),
    Payload   = proplists:get_value(<<"payload">>  , Dep),
    {Namespace, Name, Version, Payload}.

extract_dependency_list(Deps) ->
    do_edl(proplists:get_value(<<"dependencies">>, Deps), []).

do_edl([], Acc) ->
    Acc;
do_edl([H|T], Acc) ->
    Extracted = extract_dependency_data(H),
    do_edl(T, [Extracted|Acc]).

create_local_paths([]) ->
    [];
create_local_paths(Deps) when is_list(Deps) ->
    case os:getenv("HOME") of
        Home when is_list(Home) ->
            create_local_paths(Home, Deps)
    end.

create_local_paths(Home, Deps) ->
    [{Namespace, DepName, DepVsn,
      onan_file:join_paths(Home, [".onan", Namespace, DepName, DepVsn])}
     || {Namespace, DepName, DepVsn} <- Deps].

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

deploy(Config) ->
    OnanDeps     = proplists:get_value(deps, Config, []),
    OnanEndpoint = proplists:get_value(server, Config),
    Vsn          = list_to_binary(proplists:get_value(vsn, Config, "")),
    Description  = list_to_binary(proplists:get_value(description, Config, "")),
    AppName      = list_to_binary(proplists:get_value(name, Config)),
    Namespace    = list_to_binary(proplists:get_value(namespace, Config)),

    case parse_vsn(Vsn) of
        {error, invalid_vsn} ->
            %% We use semver, because reasons.
            io:format("The version supplied is invalid. "
                      "Semantic versioning is required. "
                      "Please see: http://semver.org/");
        _ ->
            {ok, Dir} = file:get_cwd(),
            ZipBytes = package_project(Dir),
            Payload = base64:encode(ZipBytes),
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
                    io:format("Not found: ~s~n", [binary_to_list(Error)]),
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
    ExceptList = [filename:join(Dir, "deps"),
                  filename:join(Dir, ".git"),
                  ".beam"],
    {ok, {_, ZipBytes}} = zip:create("",
                                     onan_file:list_relevant_files(Dir, ExceptList),
                                     [{compress, all},
                                      memory,
                                      {uncompress, [".beam", ".app"]}]),
    ZipBytes.

save_project([_|"code.zip"] = ProjPkg, Dir, Vsn) ->
    FinalPath = filename:join(Dir, Vsn),
    OutPkg = filename:join(FinalPath, "code.zip"),
    case filelib:is_file(OutPkg) of
        true ->
            io:format("ERROR: Project already exists on filesystem "
                      "refusing to overwrite~n");
        false ->
            ok = filelib:ensure_dir(OutPkg),
            ok = file:write_file(OutPkg, ProjPkg)
    end;
save_project(Payload, Dir, Vsn) when is_binary(Payload) ->
    FinalPath = onan_file:join_paths(Dir, [Vsn, "code.zip"]),
    ok = filelib:ensure_dir(FinalPath),
    ok = file:write_file(FinalPath, Payload).

home_repo(Namespace, ProjName) ->
    case os:getenv("HOME") of
        HomeDir when is_list(HomeDir) ->
            OnanHome = filename:join(HomeDir, ".onan"),
            onan_file:join_paths(OnanHome, [Namespace, ProjName])
    end.

main(["install"]) ->
    {ok, Config} = file:consult("onan.config"),
    {ok, Dir} = file:get_cwd(),
    ProjName = proplists:get_value(name, Config),
    Namespace = proplists:get_value(namespace, Config),
    Vsn = proplists:get_value(vsn, Config),
    ProjDir = home_repo(Namespace, ProjName),
    ok = filelib:ensure_dir(ProjDir),
    ProjPkg = package_project(Dir),
    ok = save_project(ProjPkg, ProjDir, Vsn);

main(["deps"]) ->
    {ok, Config} = file:consult("onan.config"),
    {ok, Dir} = file:get_cwd(),
    Deps = proplists:get_value(deps, Config),
    LocalPaths = create_local_paths(Deps),
    [begin
         case filelib:is_dir(DepDir) of
             true ->
                 {ok, _} = copy_dep(DepName, DepDir, Dir);
             false ->
                 io:format("Missing local dependency: ~s~n", [DepName]),
                 case get_remote_dependency(Namespace, DepName, DepVsn, Config) of
                     {error, notfound} ->
                         io:format("Missing remote dependency: ~s~n", [DepName]);
                     {ok, Body} ->
                         DecodedBody = jsx:decode(Body),
                         RemoteDeps = extract_dependency_list(DecodedBody),
                         ok = save_remote_deps(RemoteDeps),
                         ok = save_remote_deps_to_project(RemoteDeps)
                 end
         end
     end || {Namespace, DepName, DepVsn, DepDir} <- LocalPaths];

main(["deploy"]) ->
    {ok, Config} = file:consult("onan.config"),
    deploy(Config);

main(["list-deps"]) ->
    {ok, Config} = file:consult("onan.config"),
    Deps = proplists:get_value(deps, Config),
    [io:format("Name: ~s~nVersion: ~s~n", [Name, Vsn])
     || {Name, Vsn} <- Deps].
