%% Binary search tree.

-module(treesort).
-export([
         root/1,
         new_node/1,
         insert/2,
         children/2,
         value/1,
         sorted/1,
         convert/2,
         fold_nodes/3,
         fold_edges/3
        ]).

root(Val) ->
    new_node(Val).

new_node(Val) ->
    {Val, left, right}.


insert(NewValue, left) ->
    new_node(NewValue);
insert(NewValue, right) ->
    new_node(NewValue);
insert(NewVal, {Val, Left, Right}) when NewVal < Val ->
    {Val, insert(NewVal, Left), Right};
insert(NewVal, {Val, Left, Right}) ->
    {Val, Left, insert(NewVal, Right)}.

children(Value, {Value, L, R}) ->
    {L, R};
children(_SearchVal, left) ->
    notfound;
children(_SearchVal, right) ->
    notfound;
children(SearchVal, {Value, L, _R}) when SearchVal < Value ->
    children(SearchVal, L);
children(SearchVal, {_Value, _L, R}) ->
    children(SearchVal, R).

value(left) ->
    undefined;
value(right) ->
    undefined;
value({Value, _L, _R}) ->
    Value.

sorted(left) ->
    [];
sorted(right) ->
    [];
sorted({Value, Left, Right}) ->
    sorted(Left) ++ [Value] ++ sorted(Right).

maybe_convert(_Fun, left) ->
    left;
maybe_convert(_Fun, right) ->
    right;
maybe_convert(Fun, Node) ->
    convert(Fun, Node).

convert(F, {Value, Left, Right}) ->
    {F(Value), maybe_convert(F, Left), maybe_convert(F, Right)}.

maybe_fold_nodes(_Fun, Acc, left) ->
    Acc;
maybe_fold_nodes(_Fun, Acc, right) ->
    Acc;
maybe_fold_nodes(Fun, Acc, Node) ->
    fold_nodes(Fun, Acc, Node).

fold_nodes(F, Acc, {Value, Left, Right}) ->
    Acc2 = maybe_fold_nodes(F, Acc, Left),
    Acc3 = F(Value, Acc2),
    maybe_fold_nodes(F, Acc3, Right).


fold_edges(_F, Acc, {_NodeValue, left, right}) ->
    Acc;
fold_edges(F, Acc, {NodeValue, {ChildValue, _, _}=Child, right}) ->
    Acc2 = fold_edges(F, Acc, Child),
    F(NodeValue, ChildValue, left, Acc2);
fold_edges(F, Acc, {NodeValue, left, {ChildValue, _, _}=Child}) ->
    Acc2 = F(NodeValue, ChildValue, right, Acc),
    fold_edges(F, Acc2, Child);
fold_edges(F, Acc, {NodeValue, {LeftVal, _, _}=Left, {RightVal, _, _}=Right}) ->
    Acc2 = fold_edges(F, Acc, Left),
    Acc3 = F(NodeValue, LeftVal, left, Acc2),
    Acc4 = F(NodeValue, RightVal, right, Acc3),
    fold_edges(F, Acc4, Right).
