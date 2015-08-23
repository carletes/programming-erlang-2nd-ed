%% Exercises, chapter 12.

-module(exercises).
-export([exercise_1/0, exercise_3/0, ring_loop/1]).

%% 1. Write a function start(AnAtom, Fun) to register AnAtom as spawn(Fun).
%%    Make sure your program works correctly in the case when two parallel
%%    processes simultaneously evaluate start/2. In this case, you must
%%    guarantee that one of these processes succeeds and the other fails.
start(Name, Fun) when is_atom(Name) ->
    try register(Name, self()) of
        true ->
            Pid = spawn(Fun),
            unregister(Name),
            register(Name, Pid)
    catch
        error:badarg -> false
    end.

exercise_1() ->
    Hello = fun() -> io:format("Hello~n") end,
    true = start(hello, Hello),
    false = start(hello, Hello),
    ok.

%% 3. Write a ring benchmark. Create N processes in a ring. Send a message
%%    round the ring M times so that a total of N * M messages get sent. Time
%%    how long this takes for different values of N and M.
%%
%%    Write a similar program in some other programming language you are
%%    familiar with. Compare the results. Write a blog, and publish the results
%%    on the Internet!

ring(N, M) when N > 1, M > 0 ->
    Head = lists:foldl(fun(_, Prev) -> spawn(?MODULE, ring_loop, [Prev]) end,
                       self(),
                       lists:seq(1, N-1)),
    Head ! {msg, M},
    main_loop(Head).

ring_loop(Next) ->
    receive
        {msg, Msg} ->
            Next ! {msg, Msg},
            ring_loop(Next);
        stop ->
            Next ! stop
    end.

main_loop(Head) ->
    receive
        {msg, M} when M > 1 ->
            Head ! {msg, M-1},
            main_loop(Head);
        {msg, M} when M =:= 1 ->
            Head ! stop
    end.

exercise_3() ->
    ring(100000, 10000).
