-module(reploy).
-compile(export_all).


deps({config, _, Config, _, _, _, _}, _AppFile) ->
    ReployDeps     = proplists:get_value(reploy_deps, Config, []),
    ReployEndpoint = proplists:get_value(reploy_endpoint, Config),
    get_deps(ReployDeps, ReployEndpoint).

get_deps(_, undefined) ->
    io:format("Missing reploy endpoint. "
              "Please supply an endpoint from which to retrieve "
              "dependencies. e.g:~n\t"
              "{reply_endpoint, \"http://foobar.com\"}~n");
get_deps([], _) ->
    io:format("No dependencies~n");
get_deps(Dependencies, _Endpoint) ->
    [io:format("Retrieving: ~p~n", [Dep]) || Dep <- Dependencies],
    ok.

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
        409 ->
            {error, {conflict_detected, jsx:decode(erlang:list_to_binary(Resp))}};
        422 ->
            {error, checksum_failure};
        _ ->
            {error, {unknown_error, Status}}
    end.

pre_deploy(_, _) ->
    %% Rebar doesn't call any application:start stuff for the plugins.
    inets:start(),
    ok.

deploy({config, _, Config, _, _, _, _}, AppFile) ->
    %% The Dir passed to this handler seems to always be incorrect,
    %% rebar internally sets the cwd to the correct directory.
    {ok, Dir} = file:get_cwd(),

    ReployDeps     = proplists:get_value(reploy_deps, Config, []),
    ReployEndpoint = proplists:get_value(reploy_endpoint, Config),

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
            {ok, {_, ZipBytes}} = zip:create("",
                                             ["../" ++ filename:basename(Dir)],
                                             [{compress, all},
                                              memory,
                                              {uncompress, [".beam", ".app"]}]),
            Payload = base64:encode(ZipBytes),

            %% Create the human-readable md5 of the payload.
            << M: 128>> = crypto:hash(md5, Payload),
            Checksum = list_to_binary(integer_to_list(M, 16)),

            DeploymentMetadata =
                [{<<"namespace">>, Namespace},
                 {<<"name">>, AppName},
                 {<<"version">>, Vsn},
                 {<<"dependencies">>, to_dep_list(ReployDeps)},
                 {<<"description">>, Description},
                 {<<"checksum">>, Checksum},
                 {<<"payload">>, Payload}],
            case do_deploy(ReployEndpoint, DeploymentMetadata) of
                {ok, Location} ->
                    io:format("Successfully deployed ~s. Artefact now "
                              "permanently lives at ~s.~n", [AppName,
                                                             ReployEndpoint ++ Location]),
                    ok;
                {error, checksum_mismatch} ->
                    io:format("The server received a different "
                              "checksum than what was calculated "
                              "prior to sending. Try again, perh"
                              "aps under a more secure connection~n"),
                    ok;
                {error, {conflict_detected, Extra}} ->
                    Version = proplists:get_value(<<"version">>, Extra),
                    io:format("An artefact already exists with this "
                              "metadata or an attempt to create a "
                              "lower-versioned artefact was made.~n~n"
                              "On Server: "
                              "Version: ~p~n",
                              [binary_to_list(Version)]),
                    {error, version_conflict}
            end
    end.
