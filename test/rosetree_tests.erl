-module(rosetree_tests).
-include_lib("eunit/include/eunit.hrl").

insert_test() ->
    Root = rosetree:root("A"),
    Root2 = rosetree:insert(rosetree:new_node("B"),
                            "A", Root),
    Root3 = rosetree:insert(rosetree:new_node("C"),
                            "B", Root2),
    ["A", "B", "C"] = rosetree:find("C", Root3).

linear_distance_test() ->
    Root = rosetree:root("A"),
    Root2 = rosetree:insert(rosetree:new_node("B"),
                            "A", Root),
    Root3 = rosetree:insert(rosetree:new_node("C"),
                            "B", Root2),
    2 = rosetree:distance("A", "C", Root3),
    2 = rosetree:distance("C", "A", Root3),
    0 = rosetree:distance("B", "B", Root3).

branch_distance_test() ->
    Root = rosetree:root("A"),
    Root2 = rosetree:insert(rosetree:new_node("B"),
                            "A", Root),
    Root3 = rosetree:insert(rosetree:new_node("C"),
                            "A", Root2),
    Root4 = rosetree:insert(rosetree:new_node("D"),
                            "B", Root3),
    Root5 = rosetree:insert(rosetree:new_node("E"),
                            "C", Root4),
    Root6 = rosetree:insert(rosetree:new_node("F"),
                            "E", Root5),
    3 = rosetree:distance("F", "A", Root6),
    2 = rosetree:distance("D", "A", Root6),
    5 = rosetree:distance("D", "F", Root6).
