%% Exceptions test
-module(g_test2).

-export([test/0]).

test() ->
    test_inline_catch(),
    %test_try_catch(),
    ok.

test_inline_catch() ->
    F = fun() -> erlang:throw(test_exception) end,
    (catch F()).
