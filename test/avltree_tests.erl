-module(avltree_tests).
-include_lib("eunit/include/eunit.hrl").

%% Cheating a bit: this is effectively 3 tests wrapped into one.
%%   Create a tree
%%   Rotate it once
%%   Test the expected breadth-first traversal
%%   Test that it's still sorted
%%   Rotate it back
%%   Test that it's identical to the original
rotate_test() ->
    Inputs = [15, 9, 5, 18, 17],
    Root = treesort:bulk_tree(Inputs),
    Root2 = avltree:rotate_left(Root),
    [18, 15, 9, 17, 5] =
        lists:reverse(
          treesort:fold_nodes(fun(V, Acc) -> [V|Acc] end, [], Root2, {bfs, ltr})
         ),
    lists:sort(Inputs) == treesort:sorted(Root2),
    Root = avltree:rotate_right(Root2).

height_test() ->
    Inputs = [15, 9, 5, 18, 17],
    Root = treesort:bulk_tree(Inputs),
    2 = avltree:height_right(Root),
    0 = avltree:balance_factor(Root).

height_after_rotate_test() ->
    Inputs = [15, 9, 5, 18, 17],
    Root = treesort:bulk_tree(Inputs),
    Root2 = avltree:rotate_left(Root),
    0 = avltree:height_right(Root2),
    3 = avltree:height_left(Root2),
    -3 = avltree:balance_factor(Root2).
