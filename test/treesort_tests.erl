-module(treesort_tests).
-include_lib("eunit/include/eunit.hrl").

insert_test() ->
    Root = treesort:root(5),
    Root2 = treesort:insert(4, Root),
    {empty, empty} = treesort:children(4, Root2).

insert2_test() ->
    Root = treesort:root(5),
    Root2 = treesort:insert(4, Root),
    {Node, empty} = treesort:children(5, Root2),
    4 = treesort:value(Node).

insert3_test() ->
    Root = treesort:root(5),
    Root2 = treesort:insert(4, Root),
    {_Left, Right} = treesort:children(5, Root2),
    undefined = treesort:value(Right).

insert_dup_test() ->
    Root = treesort:root(5),
    Root2 = treesort:insert(5, Root),
    [5] = treesort:sorted(Root2).

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

reversed_test() ->
    CF = treesort:default_compfun(),
    ReversedCF = fun(X, Y) -> CF(Y, X) end,
    Root = treesort:root(5, ReversedCF),
    Root2 = treesort:insert(2, Root),
    Root3 = treesort:insert(3, Root2),
    [5, 3, 2] = treesort:sorted(Root3).

custom_type_test() ->
    CF = treesort:default_compfun(),
    MyCF = fun({_, X}, {_, Y}) -> CF(X, Y) end,
    Root = treesort:root({"b", 5}, MyCF),
    Root2 = treesort:insert({"a", 42}, Root),
    Root3 = treesort:insert({"c", 1}, Root2),
    Root4 = treesort:delete({"b", 5}, Root3),
    [{"c", 1}, {"a", 42}] = treesort:sorted(Root4).

min_test() ->
    1 == treesort:minimum(
           treesort:bulk_tree([3, 13, 1, 7, 19, 17,
                               15, 9, 5, 11])).

max_test() ->
    19 == treesort:maximum(
            treesort:bulk_tree([3, 13, 1, 7, 19, 17,
                                15, 9, 5, 11])).

bulk_test() ->
    [1, 3, 5, 7, 9, 11, 13, 15, 17, 19] =
        treesort:sorted(treesort:bulk_tree([3, 13, 1, 7, 19, 17,
                                            15, 9, 5, 11])).

bulk_insert_test() ->
    [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29] =
        treesort:sorted(
          treesort:bulk_insert([5, 21, 11, 9, 29],
                               treesort:bulk_tree([3, 13, 27, 1, 7, 19, 17,
                                                   15, 23, 25]))).

delete_test() ->
    [1, 3, 5, 7, 11, 13, 15, 17, 19, 21, 25, 27, 29] =
        treesort:sorted(
          treesort:delete(
            9,
            treesort:delete(
              23,
              treesort:bulk_insert([5, 21, 11, 9, 29],
                                   treesort:bulk_tree([3, 13, 27, 1, 7, 19, 17,
                                                       15, 23, 25]))))).

delete_missing_test() ->
    [1, 3, 5] = treesort:sorted(treesort:delete(9, treesort:bulk_tree([5, 1, 3]))).

transform_test() ->
    Root = treesort:root(5),
    Root2 = treesort:insert(2, Root),
    Root3 = treesort:insert(3, Root2),
    Transformed = treesort:transform(
                    fun(N, M) -> {treesort:value(N)*M, M} end, 3, Root3),
    [6, 9, 15] = treesort:sorted(Transformed).

fold_nodes_test() ->
    Root = treesort:root(5),
    Root2 = treesort:insert(2, Root),
    Root3 = treesort:insert(3, Root2),
    Sum = treesort:fold_nodes(fun(Value, Sum) -> Sum + Value end,
                              0, Root3),
    10 = Sum.

fold_nodes_preorder_test() ->
    Root = treesort:bulk_tree([4, 6, 5, 1, 3, 8, 2, 7]),
    [4, 1, 3, 2, 6, 5, 8, 7] =
        lists:reverse(
          treesort:fold_nodes(
            fun(Value, Acc) -> [Value|Acc] end, [], Root,
            {depth, preorder})
         ).

fold_nodes_inorder_test() ->
    Root = treesort:bulk_tree([4, 6, 5, 1, 3, 8, 2, 7]),
    [1, 2, 3, 4, 5, 6, 7, 8] =
        lists:reverse(
          treesort:fold_nodes(
            fun(Value, Acc) -> [Value|Acc] end, [], Root,
            {depth, inorder})
         ).

fold_nodes_postorder_test() ->
    Root = treesort:bulk_tree([4, 6, 5, 1, 3, 8, 2, 7]),
    [2, 3, 1, 5, 7, 8, 6, 4] =
        lists:reverse(
          treesort:fold_nodes(
            fun(Value, Acc) -> [Value|Acc] end, [], Root,
            {depth, postorder})
         ).

fold_nodes_bfs_test() ->
    Root = treesort:bulk_tree([4, 6, 5, 1, 3, 8, 2, 7]),
    [4, 1, 6, 3, 5, 8, 2, 7] =
        lists:reverse(
          treesort:fold_nodes(
            fun(Value, Acc) -> [Value|Acc] end, [], Root,
            breadth)
         ).


fold_edges_test() ->
    Root = treesort:root(5),
    Root2 = treesort:insert(2, Root),
    Root3 = treesort:insert(3, Root2),
    [{2, 3, right}, {5, 2, left}] =
        lists:sort(
          treesort:fold_edges(fun(V1, V2, Edge, Acc) ->
                                      [{V1, V2, Edge}|Acc] end,
                              [], Root3)).
