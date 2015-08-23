-module(exercises).
-export([map_search_pred/2]).
-export([test/0]).

% Exercise 2
%
% Write a function
%
%     map_search_pred(Map, Pred)
%
% that returns the first element {Key,Value} in the map for which
% Pred(Key, Value) is true.

map_search_pred(Map, Pred) when is_map(Map) ->
    hd(lists:filter(fun({K, V}) -> Pred(K, V) end, maps:to_list(Map))).

test() ->
    Map = #{foo => 42, foo => pepe, bar => pepe},
    {bar, pepe} = map_search_pred(Map, fun(_, V) -> is_atom(V) end),
    ok.
