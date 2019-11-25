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
    Converted = treesort:convert(fun(V) -> V*3 end, Root3),
    [6, 9, 15] = treesort:sorted(Converted).
