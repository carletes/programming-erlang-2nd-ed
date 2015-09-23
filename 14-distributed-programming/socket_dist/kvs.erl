-module(kvs).
-export([start/0, store/2, lookup/1]).

%%% Public API

start() ->
    register(?MODULE, spawn(fun() -> loop(maps:from_list([])) end)).

store(Key, Value) ->
    rpc({store, Key, Value}).

lookup(Key) ->
    rpc({lookup, Key}).

%%% Private functions

rpc(Request) ->
    ?MODULE ! {self(), Request},
    receive
        {?MODULE, Response} ->
            Response
    end.

loop(Values) ->
    receive
        {From, {store, Key, Value}} ->
            From ! {?MODULE, true},
            loop(maps:put(Key, Value, Values));
        {From, {lookup, Key}} ->
            Value = try maps:get(Key, Values) of
                        Val -> {ok, Val}
                    catch
                        error:{badkey, _} ->
                            undefined
                    end,
            From ! {?MODULE, Value},
            loop(Values)
    end.
