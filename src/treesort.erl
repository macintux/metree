%% Binary search tree.
-module(treesort).
-export([
         root/1,
         root/2,
         bulk_tree/1,
         bulk_tree/2,
         insert/2,
         delete/2,
         bulk_insert/2,
         children/2,
         value/1,
         sorted/1,
         transform/3,
         fold_nodes/3,
         fold_edges/3,
         default_compfun/0,
         minimum/1,
         maximum/1
        ]).

-define(CF, fun(X, Y) when X < Y -> lt;
               (X, Y) when X > Y -> gt;
               (X, X) -> equal
            end).

default_compfun() ->
    ?CF.

root(Val) ->
    root(Val, ?CF).

root(Val, CompFun) ->
    new_node(Val, CompFun).

new_node(Val, CompFun) ->
    {Val, CompFun, empty, empty}.

bulk_tree(Vals) ->
    bulk_tree(Vals, ?CF).

%% Will crash with an empty initial list, but we have no defined
%% behavior for that scenario anyway.
bulk_tree([H|T], CompFun) ->
    Root = root(H, CompFun),
    lists:foldl(fun(V, Tree) -> insert(V, Tree) end,
                Root, T).

bulk_insert(Vals, Root) ->
    lists:foldl(fun(V, Tree) -> insert(V, Tree) end,
                Root, Vals).

insert(NewVal, {_Val, CompFun, _Left, _Right}=Node) ->
    insert(NewVal, CompFun, Node).

insert(NewValue, CompFun, empty) ->
    new_node(NewValue, CompFun);
insert(NewVal, CompFun, {Val, CompFun, Left, Right}=Node) ->
    case CompFun(NewVal, Val) of
        lt ->
            {Val, CompFun, insert(NewVal, CompFun, Left), Right};
        gt ->
            {Val, CompFun, Left, insert(NewVal, CompFun, Right)};
        equal ->
            Node
    end.

delete(Val, {_Val, CompFun, _Left, _Right}=Node) ->
    delete(Val, CompFun, Node).

delete(_Val, _CompFun, empty) ->
    empty;
delete(Val, _CompFun, {Val, _CompFun, empty, empty}) ->
    empty;
delete(Val, _CompFun, {Val, _CompFun, Left, empty}) ->
    Left;
delete(Val, _CompFun, {Val, _CompFun, empty, Right}) ->
    Right;
delete(Val, CompFun, {Val, CompFun, Left, Right}) ->
    Replacement = minimum(Right),
    NewRight = delete(Replacement, CompFun, Right),
    {Replacement, CompFun, Left, NewRight};
delete(Val, CompFun, {WrongVal, CompFun, Left, Right}=Node) ->
    case CompFun(Val, WrongVal) of
        lt ->
            {WrongVal, CompFun, delete(Val, CompFun, Left), Right};
        gt ->
            {WrongVal, CompFun, Left, delete(Val, CompFun, Right)};
        equal ->
            %% The previous function head should have caught this, but
            %% perhaps the comparison function treats two different
            %% objects equally when Erlang doesn't.
            delete(WrongVal, CompFun, Node)
    end.


children(Value, {Value, _F, L, R}) ->
    {L, R};
children(_SearchVal, empty) ->
    notfound;
children(SearchVal, {Value, CompFun, L, R}=Node) ->
    case CompFun(SearchVal, Value) of
        lt ->
            children(SearchVal, L);
        gt ->
            children(SearchVal, R);
        equal ->
            %% See corresponding comment on delete/3
            children(Value, Node)
    end.

value(empty) ->
    undefined;
value({Value, _F, _L, _R}) ->
    Value.

sorted(empty) ->
    [];
sorted({Value, _F, Left, Right}) ->
    sorted(Left) ++ [Value] ++ sorted(Right).

minimum({Val, _F, empty, _R}) ->
    Val;
minimum({_Val, _F, Left, _R}) ->
    minimum(Left).

maximum({Val, _F, _L, empty}) ->
    Val;
maximum({_Val, _F, _L, Right}) ->
    maximum(Right).

transform(F, Acc, Node) ->
    {NewTree, _Acc} = real_transform(F, Acc, Node),
    NewTree.

maybe_transform(_Fun, Acc, Child) when is_atom(Child) ->
    {Child, Acc};
maybe_transform(Fun, Acc, Node) ->
    real_transform(Fun, Acc, Node).

real_transform(F, Acc, {_Val, CompFun, Left, Right}=Node) ->
    {NewValue, Acc1} = F(Node, Acc),
    {LeftTree, Acc2} = maybe_transform(F, Acc1, Left),
    {RightTree, Acc3} = maybe_transform(F, Acc2, Right),
    {{NewValue, CompFun, LeftTree, RightTree}, Acc3}.

maybe_fold_nodes(_Fun, Acc, empty) ->
    Acc;
maybe_fold_nodes(Fun, Acc, Node) ->
    fold_nodes(Fun, Acc, Node).

fold_nodes(F, Acc, {Value, _CompFun, Left, Right}) ->
    Acc2 = maybe_fold_nodes(F, Acc, Left),
    Acc3 = F(Value, Acc2),
    maybe_fold_nodes(F, Acc3, Right).


fold_edges(_F, Acc, {_NodeValue, _CompFun, empty, empty}) ->
    Acc;
fold_edges(F, Acc, {NodeValue, _CompFun, {ChildValue, _, _, _}=Child, empty}) ->
    Acc2 = fold_edges(F, Acc, Child),
    F(NodeValue, ChildValue, left, Acc2);
fold_edges(F, Acc, {NodeValue, _CompFun, empty, {ChildValue, _, _, _}=Child}) ->
    Acc2 = F(NodeValue, ChildValue, right, Acc),
    fold_edges(F, Acc2, Child);
fold_edges(F, Acc, {NodeValue, _CompFun, {LeftVal, _, _, _}=Left,
                    {RightVal, _, _, _}=Right}) ->
    Acc2 = fold_edges(F, Acc, Left),
    Acc3 = F(NodeValue, LeftVal, left, Acc2),
    Acc4 = F(NodeValue, RightVal, right, Acc3),
    fold_edges(F, Acc4, Right).
