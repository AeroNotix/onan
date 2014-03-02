-module(reploy).
-compile(export_all).


%% deps({config, _, Config, _, _, _, _}, _AppFile) ->
%%     io:format("~p~n", [Config]),
%%     ReployDeps     = proplists:get_value(reploy_deps, Config, []),
%%     ReployEndpoint = proplists:get_value(reploy_endpoint, Config),
%%     get_deps(ReployDeps, ReployEndpoint).

%% get_deps(_, undefined) ->
%%     io:format("Missing reploy endpoint. "
%%               "Please supply an endpoint from which to retrieve "
%%               "dependencies. e.g:~n\t"
%%               "{reply_endpoint, \"http://foobar.com\"}~n");
%% get_deps([], _) ->
%%     io:format("No dependencies~n");
%% get_deps(Dependencies, Endpoint) ->
%%     [io:format("Retrieving: ~p~n", [Dep]) || Dep <- Dependencies],
%%     ok.

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

deploy({config, _, Config, _, _, _, _}, AppFile) ->
    %% The Dir passed to this handler seems to always be incorrect,
    %% rebar internally sets the cwd to the correct directory.
    {ok, Dir} = file:get_cwd(),

    ReployDeps     = proplists:get_value(reploy_deps, Config, []),
    ReployEndpoint = proplists:get_value(reploy_endpoint, Config),

    {ok, [{application, AppName, AppFileContents}]}
        = file:consult(AppFile),
    Vsn = proplists:get_value(vsn, AppFileContents),
    BinaryVsn = list_to_binary(Vsn),
    case parse_vsn(BinaryVsn) of
        {error, _} ->
            %% We use semver, because reasons.
            io:format("The version supplied is invalid. "
                      "Semantic versioning is required. "
                      "Please see: http://semver.org/");
        _ ->
            {ok, {_, ZipBytes}} = zip:create("",
                                             [Dir],
                                             [{compress, all},
                                              memory,
                                              {uncompress, [".beam", ".app"]}]),
            Payload = base64:encode(ZipBytes),
            Checksum = erlang:crc32(Payload),
            DeploymentMetadata =
                [{<<"name">>, AppName},
                 {<<"version">>, BinaryVsn},
                 {<<"checksum">>, Checksum},
                 {<<"dependencies">>, to_dep_list(ReployDeps)},
                 {<<"payload">>, Payload}],
            jsx:encode(DeploymentMetadata)
    end,
    ok.
