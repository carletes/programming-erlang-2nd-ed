-module(dist_demo).
-export([rpc/4, start/1]).

%%% Public API

start(Node) ->
    spawn(Node, fun() -> loop() end).

rpc(Pid, M, F, A) ->
    Pid ! {rpc, self(), M, F, A},
    receive
        {Pid, Response} ->
            Response
    end.

%%% Private functions

loop() ->
    receive
        {rpc, From, M, F, A} ->
            From ! {self(), (catch apply(M, F, A))},
            loop()
    end.
