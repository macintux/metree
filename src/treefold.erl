-module(treefold).
-compile(export_all).

%% Options:
%%   1 Depth-first
%%     1a In-order
%%     1b Pre-order
%%     1c Post-order
%%   2 Breadth-first
%%     2a Left to right
%%     2b Right to left

maybe_fold_nodes(_Fun, Acc, empty, _NF, _Seq) ->
    Acc;
maybe_fold_nodes(Fun, Acc, Node, NF, Seq) ->
    fold_nodes(Fun, Acc, Node, NF, Seq).

fold_nodes(F, Acc, Node, {VFun, CFun}=NFuns, {depth, inorder}=Seq) ->
    Acc2 = maybe_fold_nodes(F, Acc, CFun(Node, left), NFuns, Seq),
    Acc3 = F(VFun(Node), Acc2),
    maybe_fold_nodes(F, Acc3, CFun(Node, right), NFuns, Seq);
fold_nodes(F, Acc, Node, {VFun, CFun}=NFuns, {depth, preorder}=Seq) ->
    Acc2 = F(VFun(Node), Acc),
    Acc3 = maybe_fold_nodes(F, Acc2, CFun(Node, left), NFuns, Seq),
    maybe_fold_nodes(F, Acc3, CFun(Node, right), NFuns, Seq);
fold_nodes(F, Acc, Node, {VFun, CFun}=NFuns, {depth, postorder}=Seq) ->
    Acc2 = maybe_fold_nodes(F, Acc, CFun(Node, left), NFuns, Seq),
    Acc3 = maybe_fold_nodes(F, Acc2, CFun(Node, right), NFuns, Seq),
    F(VFun(Node), Acc3);
fold_nodes(F, Acc, Node, NFuns, breadth=Seq) ->
    breadth_first(F, Acc, [Node], NFuns).

breadth_first(_F, Acc, [], _NFuns) ->
    Acc;
breadth_first(F, Acc, [H|T], {VFun, CFun}=NFuns) ->
    breadth_first(F, F(VFun(H), Acc), T ++ children(H, CFun), NFuns).

children(Node, Fun) ->
    maybe_children([Fun(Node, left),
                    Fun(Node, right)]).

maybe_children(List) ->
    lists:filter(fun(N) -> N /= empty end,
                 List).