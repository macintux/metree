-module(rosetree_tests).
-include_lib("eunit/include/eunit.hrl").

insert_test() ->
    Root = rosetree:root("A"),
    Root2 = rosetree:insert(rosetree:new_node("B"),
                            "A", Root),
    Root3 = rosetree:insert(rosetree:new_node("C"),
                            "B", Root2),
    ["A", "B"] = rosetree:find("C", Root3).
