-module(vp_l6_SUITE).
-include_lib("common_test/include/ct.hrl").


-export([all/0]).

all() ->
    [
        ct:test_case("Test insert and lookup without timeout", ?MODULE:test_insert_lookup_no_timeout),
        ct:test_case("Test insert and lookup with timeout", ?MODULE:test_insert_lookup_with_timeout),
        ct:test_case("Test delete obsolete", ?MODULE:test_delete_obsolete)
    ].


test_insert_lookup_no_timeout() ->

    vp_l6:create(my_table),

    vp_l6:insert(my_table, key1, "value1"),

    Value = vp_l6:lookup(my_table, key1),
    ?assertEqual("value1", Value).


test_insert_lookup_with_timeout() ->
    vp_l6:create(my_table),
    vp_l6:insert(my_table, key2, "value2", 5),
    ValueBeforeTimeout = vp_l6:lookup(my_table, key2),
    ?assertEqual("value2", ValueBeforeTimeout),
    timer:sleep(10),
    ValueAfterTimeout = vp_l6:lookup(my_table, key2),
    ?assertEqual(undefined, ValueAfterTimeout).

test_delete_obsolete() ->
    vp_l6:create(my_table),
    vp_l6:insert(my_table, key3, "value3", 1),
    ValueBeforeTimeout = vp_l6:lookup(my_table, key3),
    ?assertEqual("value3", ValueBeforeTimeout),
    timer:sleep(2),
    ValueAfterTimeout = vp_l6:lookup(my_table, key3),
    ?assertEqual(undefined, ValueAfterTimeout).
