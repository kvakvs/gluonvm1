-module(init).
-export([test/0
        , my_last/1, my_last2/1]).

test() -> recurse(10).

recurse(X) when X > 0 -> recurse(X-1);
recurse(X) -> X.

%% From 99 problems: P01 (*) Find the last box of a list.
%%     Example:
%%     * (my-last '(a b c d))
%%     (D)
%% With using list reverse
my_last([])->
    false;
my_last([H|[]])->
    H;
my_last(L) ->
    R = lists:reverse(L),
    [H|_T] = R,
    H.
%% Without using list reverse
my_last2([]) ->
    false;
my_last2([H|[]]) ->
    H;
my_last2([_H|T]) when length(T) == 1 -> 
    [H1|[]] = T,
    H1;
my_last2([_H|T]) ->
    my_last2(T).
