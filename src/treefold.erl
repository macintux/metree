-module(treefold).
-compile(export_all).

%% Options:
%%   1 Depth-first
%%     1a In-order
%%        {dfs, inorder}
%%     1b Pre-order
%%        {dfs, preorder}
%%     1c Post-order
%%        {dfs, postorder}
%%   2 Breadth-first
%%     2a Left to right
%%        {bfs, ltr}
%%     2b Right to left
%%        {bfs, rtl}

maybe_fold_nodes(_Fun, Acc, empty, _NF, _Seq) ->
    Acc;
maybe_fold_nodes(Fun, Acc, Node, NF, Seq) ->
    fold_nodes(Fun, Acc, Node, NF, Seq).

fold_nodes(F, Acc, Node, {VFun, CFun}=NFuns, {dfs, inorder}=Seq) ->
    Acc2 = maybe_fold_nodes(F, Acc, CFun(Node, left), NFuns, Seq),
    Acc3 = F(VFun(Node), Acc2),
    maybe_fold_nodes(F, Acc3, CFun(Node, right), NFuns, Seq);
fold_nodes(F, Acc, Node, {VFun, CFun}=NFuns, {dfs, preorder}=Seq) ->
    Acc2 = F(VFun(Node), Acc),
    Acc3 = maybe_fold_nodes(F, Acc2, CFun(Node, left), NFuns, Seq),
    maybe_fold_nodes(F, Acc3, CFun(Node, right), NFuns, Seq);
fold_nodes(F, Acc, Node, {VFun, CFun}=NFuns, {dfs, postorder}=Seq) ->
    Acc2 = maybe_fold_nodes(F, Acc, CFun(Node, left), NFuns, Seq),
    Acc3 = maybe_fold_nodes(F, Acc2, CFun(Node, right), NFuns, Seq),
    F(VFun(Node), Acc3);
fold_nodes(F, Acc, Node, NFuns, {bfs, Dir}) ->
    breadth_first(F, Acc, [Node], NFuns, Dir).

breadth_first(_F, Acc, [], _NFuns, _Dir) ->
    Acc;
breadth_first(F, Acc, [H|T], {VFun, CFun}=NFuns, Dir) ->
    breadth_first(F, F(VFun(H), Acc), T ++ children(H, CFun, Dir), NFuns, Dir).

children(Node, Fun, ltr) ->
    maybe_children([Fun(Node, left),
                    Fun(Node, right)]);
children(Node, Fun, rtl) ->
    maybe_children([Fun(Node, right),
                    Fun(Node, left)]).


maybe_children(List) ->
    lists:filter(fun(N) -> N /= empty end,
                 List).
