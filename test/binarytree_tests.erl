-module(binarytree_tests).
-include_lib("eunit/include/eunit.hrl").

insert_test() ->
    Root = binarytree:root(5),
    Root2 = binarytree:insert(4, [left], Root),
    4 = binarytree:retrieve([left], Root2).

insert_inline_test() ->
    Root = binarytree:root(5),
    Root2 = binarytree:insert(4, [left], Root),
    Root3 = binarytree:insert(3, [left], Root2),
    4 = binarytree:retrieve([left, left], Root3).
