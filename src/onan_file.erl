-module(onan_file).


-export([join_paths/2]).


join_paths(Root, []) ->
    Root;
join_paths(Root, [H|T]) ->
    join_paths(filename:join(Root, H), T).
