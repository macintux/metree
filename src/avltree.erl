%% AVL functions for a binary search tree.
-module(avltree).
-export([
         rotate_left/1,
         rotate_right/1
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
