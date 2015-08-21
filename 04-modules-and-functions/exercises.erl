-module(exercises).
-export([test/0]).

% Exercise 2
my_tuple_to_list({}) -> [];
my_tuple_to_list(T) when is_tuple(T) ->
    [element(I, T) || I <- lists:seq(1, tuple_size(T))].

% Exercise 3
my_time_func(F) ->
    Start = erlang:monotonic_time(),
    F(),
    Stop = erlang:monotonic_time(),
    Stop - Start.

my_date_string() ->
    my_date_string(erlang:date(), erlang:time()).

my_date_string({Y, M, D}, {H, Min, S}) ->
    integer_to_list(Y) ++ "-" ++ integer_to_list(M) ++ "-" ++ integer_to_list(D) ++ " " ++
        integer_to_list(H) ++ ":" ++ integer_to_list(Min) ++ ":" ++ integer_to_list(S).

test() ->
    % Exercise 2
    [] = my_tuple_to_list({}),
    [a, b, c] = my_tuple_to_list({a, b, c}),
    % Exercise 3
    true = (0 =< my_time_func(fun() -> 42 end)),
    "1975-1-14 1:16:59" = my_date_string({1975, 1, 14}, {1,16,59}),
    ok.
