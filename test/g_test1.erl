-module(g_test1).
-export([test1/0
        %, test2/0, f_test/0
        %, my_last/1
        %, my_last2/1
        ]).

test1() -> recurse(10).
%test2() -> my_last([1,2,3,4]).
%f_test() ->
%    F = fun(X) -> X * 2 end,
%    F(2).

recurse(X) when X > 0 -> recurse(X-1);
recurse(X) -> X.

%% From 99 problems: P01 (*) Find the last box of a list.
%%     Example:
%%     * (my-last '(a b c d))
%%     (D)
%% With using list reverse
%my_last([])->
%    false;
%my_last([H|[]])->
%    H;
%my_last(L) ->
%    R = lists:reverse(L),
%    [H|_T] = R,
%    H.

%% Without using list reverse
%my_last2([]) ->
%    false;
%my_last2([H|[]]) ->
%    H;
%my_last2([_H|T]) when length(T) == 1 -> 
%    [H1|[]] = T,
%    H1;
%my_last2([_H|T]) ->
%    my_last2(T).
