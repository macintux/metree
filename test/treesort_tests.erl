-module(treesort_tests).
-include_lib("eunit/include/eunit.hrl").

insert_test() ->
    Root = treesort:root(5),
    Root2 = treesort:insert(4, Root),
    {left, right} = treesort:children(4, Root2).

insert2_test() ->
    Root = treesort:root(5),
    Root2 = treesort:insert(4, Root),
    {Node, right} = treesort:children(5, Root2),
    4 = treesort:value(Node).

insert3_test() ->
    Root = treesort:root(5),
    Root2 = treesort:insert(4, Root),
    {_Left, Right} = treesort:children(5, Root2),
    undefined = treesort:value(Right).

empty_test() ->
    Root = treesort:root(5),
    notfound = treesort:children(4, Root).

sorted_test() ->
    Root = treesort:root(5),
    Root2 = treesort:insert(2, Root),
    Root3 = treesort:insert(3, Root2),
    [2, 3, 5] = treesort:sorted(Root3).

sorted2_test() ->
    Root = treesort:root(5),
    [5] = treesort:sorted(Root).

convert_test() ->
    Root = treesort:root(5),
    Root2 = treesort:insert(2, Root),
    Root3 = treesort:insert(3, Root2),
    {Converted, _Acc} = treesort:convert(fun({V, _L, _R}, M) -> {V*M, M} end, 3, Root3),
    [6, 9, 15] = treesort:sorted(Converted).

fold_nodes_test() ->
    Root = treesort:root(5),
    Root2 = treesort:insert(2, Root),
    Root3 = treesort:insert(3, Root2),
    Sum = treesort:fold_nodes(fun(Value, Sum) -> Sum + Value end,
                              0, Root3),
    10 = Sum.

fold_edges_test() ->
    Root = treesort:root(5),
    Root2 = treesort:insert(2, Root),
    Root3 = treesort:insert(3, Root2),
    [{2, 3, right}, {5, 2, left}] =
        lists:sort(
          treesort:fold_edges(fun(V1, V2, Edge, Acc) ->
                                      [{V1, V2, Edge}|Acc] end,
                              [], Root3)).
