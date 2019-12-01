%% AVL functions for a binary search tree.
-module(avltree).
-export([
         rotate_left/1,
         rotate_right/1,
         height_left/1,
         height_right/1,
         balance_factor/1
        ]).

%% For the rotation functions, A and B nodes are named based on the
%% illustration at
%% http://pages.cs.wisc.edu/~ealexand/cs367/NOTES/AVL-Trees/index.html
%%
%% When rotating left, B replaces A as the root of this subtree.
%% When rotating right, A replaces B.
rotate_left({ValA, CompFun, LeftA,
             {ValB, CompFun, LeftB, RightB}
            }) ->
    {ValB, CompFun, {ValA, CompFun, LeftA, LeftB}, RightB}.

rotate_right({ValB, CompFun,
              {ValA, CompFun, LeftA, RightA},
              RightB}) ->
    {ValA, CompFun, LeftA, {ValB, CompFun, RightA, RightB}}.

height_left({_Val, _CompFun, empty, _Right}) ->
    0;
height_left({_Val, _CompFun, Left, _Right}) ->
    height(Left).

height_right({_Val, _CompFun, _Left, empty}) ->
    0;
height_right({_Val, _CompFun, _Left, Right}) ->
    height(Right).

height(empty) ->
    0;
height({_Val, _CompFun, Left, Right}) ->
    1 + max(height(Left), height(Right)).

balance_factor(Node) ->
    height_right(Node) - height_left(Node).
