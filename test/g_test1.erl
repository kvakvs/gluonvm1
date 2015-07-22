-module(g_test1).
-export([test1/0
        , test2/0
        %, f_test/0
        %, my_last/1
        %, my_last2/1
        ]).

test1() -> recurse(10).
test2() -> 
    X = [1,2,3,4,5],
    5 = my_last2(X),
    4 = my_but_last2(X),
    2 = element_at(2, X),
    5 = len(X),
    [5,4,3,2,1] = rev(X),
    false = is_palindrome([]),
    false = is_palindrome(X),
    true = is_palindrome([1,2,3,2,1]).

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

%% From 99 problems: P03 Find the K'th element of a list.
%% Find the K'th element of a list (1-based)
element_at(K,L) when length(L) < K -> false;
element_at(K,L)-> element_at(K,L,1).
element_at(K,[H|_T],C) when C == K-> H;
element_at(K,[_H|T],C) -> element_at(K,T,C+1).

%% From 99 problems: P04 Find the number of elements of a list.
len([])-> 0;
len(L) -> len(L,0).

len([],Count) -> Count;
len([_H|T],Count)-> len(T,Count+1).

%% From 99 problems: P05 Reverse a list.
rev([])-> [];
rev(L) -> rev(L,[]).
rev([],R)-> R;
rev([H|T],R)-> rev(T,[H|R]).

%% P06 Find out whether a list is a palindrome.
is_palindrome([])-> false;
is_palindrome(L) when length(L) == 1 -> false;
is_palindrome(L) -> case L == rev(L) of true -> true; false -> false end.
