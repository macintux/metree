%% Binary tree.
%%
%% The intent for this module is to explore the possibilities for
%% building a binary search tree using this code, by choosing another
%% type of binary tree and doing the same to identify commonalities.
-module(binarytree).
-export([
         root/1,
         insert/3,
         retrieve_path/2,
         retrieve_all/1
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

retrieve_path([], {Val, _Left, _Right}) ->
    Val;
retrieve_path([left|Tail], {_Val, Left, _Right}) ->
    retrieve_path(Tail, Left);
retrieve_path([right|Tail], {_Val, _Left, Right}) ->
    retrieve_path(Tail, Right).

retrieve_all(empty) ->
    [];
retrieve_all({Val, Left, Right}) ->
    [Val] ++ retrieve_all(Left) ++ retrieve_all(Right).
