-module(binarytree_tests).
-include_lib("eunit/include/eunit.hrl").

insert_test() ->
    Root = binarytree:root(5),
    Root2 = binarytree:insert(4, [left], Root),
    4 = binarytree:retrieve_path([left], Root2).

insert_inline_test() ->
    Root = binarytree:root(5),
    Root2 = binarytree:insert(4, [left], Root),
    Root3 = binarytree:insert(3, [left], Root2),
    4 = binarytree:retrieve_path([left, left], Root3).

depth_first_retrieve_test() ->
    Root = binarytree:root(5),
    Root2 = binarytree:insert(4, [left], Root),
    Root3 = binarytree:insert(3, [left], Root2),
    Root4 = binarytree:insert(6, [right], Root3),
    Root5 = binarytree:insert(2, [right, left], Root4),
    Root6 = binarytree:insert(1, [right, right], Root5),
    [5, 3, 4, 6, 2, 1] = binarytree:retrieve_all(Root6).
