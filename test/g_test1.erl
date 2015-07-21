-module(g_test1).
-export([test1/0
        , test2/0
        %, f_test/0
        %, my_last/1
        %, my_last2/1
        ]).

test1() -> recurse(10).
test2() -> 
    X = [1,2,3,4],
    4 = my_last2(X),
    3 = my_but_last2(X).

%f_test() ->
%    F = fun(X) -> X * 2 end,
%    F(2).

recurse(X) when X > 0 -> recurse(X-1);
recurse(X) -> X.

%% From 99 problems: P01 Find the last box of a list.
%% Variant without using list reverse
my_last2([]) -> false;
my_last2([H|[]]) -> H;
my_last2([_H|T]) when length(T) == 1 ->
  [H1|[]] = T,
  H1;
my_last2([_H|T]) ->
  my_last2(T).

%% From 99 problems: P02 Find the last but one box of a list.
%% Variant without using list reverse 
my_but_last2([])-> false;
my_but_last2([_H|[]]) -> false;
my_but_last2([H|T]) when length(T) == 1 -> H;
my_but_last2([_H|T]) -> my_but_last2(T).
