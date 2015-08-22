-module(math_functions).
-export([even/1, filter/2, odd/1, split_acc/1, split_filter/1]).

even(X) when is_integer(X) ->
    X rem 2 =:= 0.

odd(X) when is_integer(X) ->
    X rem 2 =:= 1.

filter({M, F}, L) ->
    [X || X <-L, M:F(X) =:= true].

split_acc(L) ->
    split_acc(L, [], []).

split_acc([], Even, Odd) ->
    {Even, Odd};
split_acc([X|T], Even, Odd) ->
    IsEven = even(X),
    if IsEven ->
            split_acc(T, [X|Even], Odd);
       true ->
            split_acc(T, Even, [X|Odd])
    end.

split_filter(L) ->
    {[X || X <- L, even(X)], [Y || Y <- L, odd(Y)]}.
