-module(shop).
-export([cost/1, total/1]).

cost(oranges) ->
    5;
cost(newspaper) ->
    8;
cost(apples) ->
    2;
cost(pears) ->
    9;
cost(milk) ->
    7.

total(L) ->
    lists:sum([N * cost(What) || {What, N} <- L]).
