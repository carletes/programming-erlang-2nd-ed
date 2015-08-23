% Exercise 3.
%
% Look up the manual pages for the Ruby hash class. Make a module of the
% methods in the Ruby class that you think would be appropriate to Erlang.

-module(hash).
-export([new/0,
         any/2,
         assoc/2,
         clear/1,
         each/2]).

new() ->
    maps:new().

any(Pred, Map) ->
    lists:any(fun({K,  V}) -> Pred(K, V) end, maps:to_list(Map)).

assoc(Map, Term) ->
    case maps:find(Term, Map) of
        {ok, Value} ->
            {ok, {Term, Value}};
        error ->
            error
    end.

clear(_Map) ->
    new().

each(Func, Map) ->
    lists:map(Func, maps:to_list(Map)).
