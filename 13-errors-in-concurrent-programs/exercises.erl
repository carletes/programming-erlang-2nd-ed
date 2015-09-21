-module(exercises).
-export([exercise1/0, exercise3/0, exercise4/0]).
-export([still_running/0, wait/1]).

%% 1. Write a function my_spawn(Mod, Func, Args) that behaves like
%% spawn(Mod, Func, Args) but with one difference. If the spawned process dies,
%% a message should be printed saying why the process died and how long the
%% process lived for before it died.

my_spawn(Mod, Func, Args) ->
    Pid = spawn(Mod, Func, Args),
    Start = erlang:monotonic_time(),
    lib_misc:on_exit(Pid,
                     fun(Why) ->
                             Stop = erlang:monotonic_time(),
                             Elapsed = erlang:convert_time_unit(Stop - Start,
                                                                native,
                                                                nano_seconds),
                             io:format("~p died after ~p ns, reason: ~p~n",
                                       [Pid, Elapsed, Why])
                     end),
    Pid.

wait(Delay) ->
    receive
    after Delay ->
            true
    end.

exercise1() ->
    my_spawn(?MODULE, wait, [1000]).

%% 3. Write a function my_spawn(Mod, Func, Args, Time) that behaves like
%% spawn(Mod, Func, Args) but with one difference. If the spawned process lives
%% for more than Time seconds, it should be killed.

my_spawn(Mod, Func, Args, Time) ->
    Pid = my_spawn(Mod, Func, Args),
    Delay = Time * 1000,
    spawn(fun() ->
                  io:format("Waiting ~p ms for ~p~n", [Delay, Pid]),
                  receive
                  after Delay ->
                          io:format("Killing ~p~n", [Pid]),
                          exit(Pid, timeout)
                  end
          end),
    Pid.

exercise3() ->
    my_spawn(?MODULE, wait, [2000], 1),
    my_spawn(?MODULE, wait, [500], 1).

%% 4. Write a function that creates a registered process that writes out
%% "I'm still running" every five seconds. Write a function that monitors this
%% process and restarts it if it dies. Start the global process and the monitor
%% process. Kill the global process and check that it has been restarted by the
%% monitor process.

still_running() ->
    receive
        Any ->
            io:format("~p: Got ~p, leaving~n", [self(), Any])
    after 5000 ->
            io:format("~p: Still running~n", [self()]),
            still_running()
    end.

keep_alive(RegName, Pid, Ref, Mod, Func, Args) ->
    io:format("keep_alive(): Monitoring Pid ~p, Ref ~p~n", [Pid, Ref]),
    receive
        {'DOWN', Ref, process, Pid, Why} ->
            io:format("~p died (~p), restarting~n", [Pid, Why]),
            {NewPid, NewRef} = spawn_monitor(Mod, Func, Args),
            register(RegName, NewPid),
            keep_alive(RegName, NewPid, NewRef, Mod, Func, Args);
        Any ->
            io:format("Unexpected message: ~p~n", [Any]),
            keep_alive(RegName, Pid, Ref, Mod, Func, Args)
    end.

spawn_monitored(Mod, Func, Args, RegName) ->
    Me = self(),
    Monitor = spawn(fun() ->
                            {Pid, Ref} = spawn_monitor(Mod, Func, Args),
                            register(RegName, Pid),
                            Me ! {child, Pid},
                            keep_alive(RegName, Pid, Ref, Mod, Func, Args)
                    end),
    receive
        {child, Pid} ->
            {Pid, Monitor}
    end.

exercise4() ->
    {Pid, Monitor} = spawn_monitored(?MODULE, still_running, [], exercise4_process),

    io:format("Waiting to kill ~p...~n", [Pid]),
    receive
    after 10000 ->
            exit(Pid, pepe)
    end,

    io:format("... done, waiting to kill monitor ~p...~n", [Monitor]),
    receive
    after 10000 ->
            exit(Monitor, pepe)
    end,

    exercise4_process ! go_away_now.
