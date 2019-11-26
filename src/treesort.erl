%% Binary search tree.

-module(treesort).
-export([
         root/1,
         root/2,
         new_node/1,
         new_node/2,
         insert/2,
         children/2,
         value/1,
         sorted/1,
         convert/3,
         fold_nodes/3,
         fold_edges/3
        ]).

root(Val) ->
    root(Val, fun(X, Y) -> X < Y end).

root(Val, CompFun) ->
    new_node(Val, CompFun).

new_node(Val) ->
    new_node(Val, fun(X, Y) -> X < Y end).

new_node(Val, CompFun) ->
    {Val, CompFun, left, right}.


insert(NewVal, {_Val, CompFun, _Left, _Right}=Node) ->
    insert(NewVal, CompFun, Node).

insert(NewValue, CompFun, left) ->
    new_node(NewValue, CompFun);
insert(NewValue, CompFun, right) ->
    new_node(NewValue, CompFun);
insert(NewVal, CompFun, {Val, CompFun, Left, Right}) ->
    %% The two CompFun instances should be the same function; if not,
    %% I want this to error anyway.
    case CompFun(NewVal, Val) of
        true ->
            {Val, CompFun, insert(NewVal, CompFun, Left), Right};
        false ->
            {Val, CompFun, Left, insert(NewVal, CompFun, Right)}
    end.

children(Value, {Value, _F, L, R}) ->
    {L, R};
children(_SearchVal, left) ->
    notfound;
children(_SearchVal, right) ->
    notfound;
children(SearchVal, {Value, CompFun, L, R}) ->
    case CompFun(SearchVal, Value) of
        true ->
            children(SearchVal, L);
        false ->
            children(SearchVal, R)
    end.

value(left) ->
    undefined;
value(right) ->
    undefined;
value({Value, _F, _L, _R}) ->
    Value.

sorted(left) ->
    [];
sorted(right) ->
    [];
sorted({Value, _F, Left, Right}) ->
    sorted(Left) ++ [Value] ++ sorted(Right).

convert(F, Acc, Node) ->
    {NewTree, _Acc} = real_convert(F, Acc, Node),
    NewTree.

maybe_convert(_Fun, Acc, Child) when is_atom(Child) ->
    {Child, Acc};
maybe_convert(Fun, Acc, Node) ->
    real_convert(Fun, Acc, Node).

real_convert(F, Acc, {_Val, CompFun, Left, Right}=Node) ->
    {NewValue, Acc1} = F(Node, Acc),
    {LeftTree, Acc2} = maybe_convert(F, Acc1, Left),
    {RightTree, Acc3} = maybe_convert(F, Acc2, Right),
    {{NewValue, CompFun, LeftTree, RightTree}, Acc3}.

maybe_fold_nodes(_Fun, Acc, left) ->
    Acc;
maybe_fold_nodes(_Fun, Acc, right) ->
    Acc;
maybe_fold_nodes(Fun, Acc, Node) ->
    fold_nodes(Fun, Acc, Node).

fold_nodes(F, Acc, {Value, _CompFun, Left, Right}) ->
    Acc2 = maybe_fold_nodes(F, Acc, Left),
    Acc3 = F(Value, Acc2),
    maybe_fold_nodes(F, Acc3, Right).


fold_edges(_F, Acc, {_NodeValue, _CompFun, left, right}) ->
    Acc;
fold_edges(F, Acc, {NodeValue, _CompFun, {ChildValue, _, _, _}=Child, right}) ->
    Acc2 = fold_edges(F, Acc, Child),
    F(NodeValue, ChildValue, left, Acc2);
fold_edges(F, Acc, {NodeValue, _CompFun, left, {ChildValue, _, _, _}=Child}) ->
    Acc2 = F(NodeValue, ChildValue, right, Acc),
    fold_edges(F, Acc2, Child);
fold_edges(F, Acc, {NodeValue, _CompFun, {LeftVal, _, _, _}=Left,
                    {RightVal, _, _, _}=Right}) ->
    Acc2 = fold_edges(F, Acc, Left),
    Acc3 = F(NodeValue, LeftVal, left, Acc2),
    Acc4 = F(NodeValue, RightVal, right, Acc3),
    fold_edges(F, Acc4, Right).
