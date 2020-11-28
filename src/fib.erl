-module(fib).
-export([fib/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

fib(0) ->
    1;
fib(N) when is_integer(N), N > 0 ->
    fib(N, 0, 1).

fib(0, _X, Y) ->
    Y;
fib(N, X, Y) ->
    fib(N - 1, Y, X + Y).

-ifdef(TEST).

fib_test_() -> [
    ?_assertEqual(fib(0), 1),
    ?_assertEqual(fib(1), 1),
    ?_assertEqual(fib(2), 2),
    ?_assertEqual(fib(3), 3),
    ?_assertEqual(fib(4), 5),
    ?_assertEqual(fib(10) + fib(11), fib(12))
].

-endif.