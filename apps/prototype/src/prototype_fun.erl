%%%-------------------------------------------------------------------
%%% @doc Function object
%%% @end
%%%-------------------------------------------------------------------
-module(prototype_fun).

%% API
-export([new/1]).

new({M, F, Arity}) ->
  {funobject, #{mfarity => {M, F, Arity}}}.
