%% Transform one of my sorted binary trees into a graphviz document.
-module(tree2gv).
-export([
         tree2gv/1,
         tree2gv/2
        ]).


head() ->
    "digraph erlang_tree {\n    node [shape = Mrecord];\n".

tail() ->
    "}\n".

fmt(Fmt, Params) ->
    lists:flatten(io_lib:format(Fmt, Params)).

to_gv_node(Node, Idx) ->
    Value = treesort:value(Node),
    Name = fmt("N_~B", [Idx]),
    Label = fmt("[label = \"<left> |<value> ~B|<right> \"]", [Value]),
    {{Name, Label}, Idx+1}.

node_fold({Name, Label}, Acc) ->
    [fmt("    ~s ~s;", [Name, Label])|Acc].

edge_fold({N1, _L1}, {N2, _L2}, left, Acc) ->
    [fmt("    ~s:left -> ~s:value;", [N1, N2])|Acc];
edge_fold({N1, _L1}, {N2, _L2}, right, Acc) ->
    [fmt("    ~s:right -> ~s:value;", [N1, N2])|Acc].

%% Multiple stages:
%%   1) Transform a sorted tree into a tree with dot node definitions as
%%      values
%%   2) Navigate the tree again to retrieve that list of nodes
%%   3) Navigate the tree again to generate a list of edges
tree2gv(Tree) ->
    Transformed = treesort:transform(fun to_gv_node/2, 0, Tree),
    Nodes = treesort:fold_nodes(fun node_fold/2, [], Transformed),
    Edges = treesort:fold_edges(fun edge_fold/4, [], Transformed),
    lists:flatten(head() ++ lists:join("\n", Nodes) ++ "\n" ++
                      lists:join("\n", Edges) ++ "\n" ++ tail()).

tree2gv(Tree, File) ->
    GV = tree2gv(Tree),
    file:write(File, GV).
