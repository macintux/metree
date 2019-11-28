%% Binary tree.
-module(binarytree).
-export([
         root/1,
         insert/3,
         retrieve/2
        ]).

root(Val) ->
    new_node(Val).

new_node(Val) ->
    {Val, empty, empty}.

new_node(Val, Left, Right) ->
    {Val, Left, Right}.

insert(NewVal, [left], {Val, Left, Right}) ->
    {Val, new_node(NewVal, Left, empty), Right};
insert(NewVal, [right], {Val, Left, Right}) ->
    {Val, Left, new_node(NewVal, empty, Right)};
insert(NewVal, [left|PathTail], {Val, Left, Right}) ->
    {Val, insert(NewVal, PathTail, Left), Right};
insert(NewVal, [right|PathTail], {Val, Left, Right}) ->
    {Val, Left, insert(NewVal, PathTail, Right)}.

retrieve([], {Val, _Left, _Right}) ->
    Val;
retrieve([left|Tail], {_Val, Left, _Right}) ->
    retrieve(Tail, Left);
retrieve([right|Tail], {_Val, _Left, Right}) ->
    retrieve(Tail, Right).
