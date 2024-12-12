-module(vp_l6_cache).
-behaviour(gen_server).

-export([start_link/2, init/1, handle_call/3, handle_info/2, terminate/2]).

-define(DEFAULT_TIMEOUT, 600).

start_link(Name, TableName) ->
    gen_server:start_link({local, Name}, vp_l6_cache, [TableName]).

init([TableName]) ->
    Table = ets:new(TableName, [set, named_table]),
    send(self(), delete_obsolete),
    {ok, Table}.

handle_call({insert, Key, Value}, _From, Table) ->
    ets:insert(Table, {Key, {Value, undefined}}),
    {reply, ok, Table};

handle_call({insert, Key, Value, Timeout}, _From, Table) ->
    ExpiryTime = calendar:local_time() + Timeout,
    ets:insert(Table, {Key, {Value, ExpiryTime}}),
    {reply, ok, Table};

handle_call({lookup, Key}, _From, Table) ->
    case ets:lookup(Table, Key) of
        [{_, {Value, ExpiryTime}}] when ExpiryTime == undefined -> 
            {reply, Value, Table};
        [{_, {Value, ExpiryTime}}] when ExpiryTime > calendar:local_time() -> 
            {reply, Value, Table};
        _ -> 
            {reply, undefined, Table}
    end.

handle_info(delete_obsolete, Table) ->
    delete_obsolete(Table),
    send(self(), delete_obsolete),
    {noreply, Table};

terminate(_Reason, _State) ->
    ok.

delete_obsolete(Table) ->
    Now = calendar:local_time(),
    ets:match_delete(Table, {{_, {'$1', '$2'}}, [{'<', Now, '$2'}]}).
