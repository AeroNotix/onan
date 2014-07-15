-module(onan_file).


-export([join_paths/2]).
-export([list_relevant_files/2]).


join_paths(Root, []) ->
    Root;
join_paths(Root, [H|T]) ->
    join_paths(filename:join(Root, H), T).

any_match(S, All) ->
    lists:any(fun(P) ->
                      string:str(S, P) =/= 0
              end, All).

list_relevant_files(Root, Except) ->
    Paths = filelib:wildcard(Root ++ "/**"),
    Matcher =
        fun(S) ->
                case any_match(S, Except)
                of true ->
                        false;
                    false ->
                        true
                end
        end,
    [lists:nthtail(length(Root) + 1, Path)
     || Path <- lists:filter(Matcher, Paths)].
