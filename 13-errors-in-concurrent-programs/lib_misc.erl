-module(lib_misc).
-export([on_exit/2]).

on_exit(Pid, Func) ->
    spawn(fun() ->
                  Ref = monitor(process, Pid),
                  receive
                      {'DOWN', Ref, process, Pid, Why} ->
                          Func(Why)
                  end
          end).
