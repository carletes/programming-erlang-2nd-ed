-module(try_test).
-export([demo1/0, demo2/0]).

generate_exception(1) -> a;
generate_exception(2) -> throw(a);
generate_exception(3) -> exit(a);
generate_exception(4) -> {'EXIT', a};
generate_exception(5) -> error(a).

catcher1(N) ->
    try generate_exception(N) of
        Val -> {N, normal, Val}
    catch
        throw:X -> {N, caught, thrown, X};
        exit:X -> {N, caught, exited, X};
        error:X -> {N, caught, error, X}
    end.

catcher2(N) ->
    catch generate_exception(N).

demo1() ->
     [catcher1(I) || I <- lists:seq(1, 5)].

demo2() ->
    [{I, catcher2(I)} || I <- lists:seq(1, 5)].
