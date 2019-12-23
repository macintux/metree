%% Rose tree (tree with arbitrary number of children).
%%
%% There is no sorting, only parent/child relationships, and each node
%% has a value associated with it (the "Label" here). The code will
%% not work correcgtly if two labels are intended to be equal but are
%% not the same where Erlang is concerned.
%%
%% Wrote this in a hurry for day 6 of 2019 Advent of Code.
%%
%% XXX: add height/1, depth/2? depth/2 is the same as distance/3 when
%% one node is the root, or at least it should be.
-module(rosetree).
-export([
         root/1,
         new_node/1,
         insert/3,
         find/2,
         cumulative_height/1,
         distance/3
        ]).

root(Label) ->
    new_node(Label).

new_node(Label) ->
    {Label, []}.

insert(NewNode, Under, {Under, Children}) ->
    {Under, [NewNode|Children]};
insert(NewNode, Under, {OtherLabel, Children}) ->
    {OtherLabel, lists:map(fun(C) -> insert(NewNode, Under, C) end,
                           Children)}.

%% Calculate the height of every node; the combined heights form the
%% number of valid paths in this system.
cumulative_height(Tree) ->
    cumulative_height(Tree, 0).

cumulative_height({_Label, Children}, Height) ->
    Height + lists:sum(lists:map(fun(C) -> cumulative_height(C, Height+1) end,
                                 Children)).

%% Find the common ancestor of two nodes, and the path lengths below
%% that ancestor, when combined, are the distance.
distance(L1, L2, Tree) ->
    Path1 = find(L1, Tree),
    Path2 = find(L2, Tree),

    %% I stumbled upon this Erlang list subtraction behavior by dumb
    %% luck. Example:
    %% > [1, 2, 3, 4] -- [1, 2, 5, 6].
    %% [3,4]
    Path1Unique = Path1 -- Path2,
    Path2Unique = Path2 -- Path1,
    length(Path1Unique) + length(Path2Unique).

find(Label, Tree) ->
    lists:reverse(find(Label, [Tree], [])).

find(_Label, [], _Acc) ->
    [];
find(Label, [{Label, _Children}|_T], Acc) ->
    %% Return the path to, but not including, the matched node value
    Acc;
find(Label, [{OtherLabel, [C1|CT]}|T], Acc) ->
    %% Ordinarily I prefer to use function heads instead of case
    %% statements, *especially* nested case statements, but this logic
    %% is too confusing for me when broken out into new functions.
    %%
    %% Chase the first child, then the rest of OtherLabel's children,
    %% then finally OtherLabel's siblings. Depth-first search.
    case find(Label, [C1], [OtherLabel|Acc]) of
        [] ->
            case find(Label, CT, [OtherLabel|Acc]) of
                [] ->
                    find(Label, T, Acc);
                Found ->
                    Found
            end;
        Found ->
            Found
    end;
find(_Label, [{_OtherLabel, []}|_T], _Acc) ->
    [].
