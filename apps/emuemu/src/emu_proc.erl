%%%-------------------------------------------------------------------
%%% @doc Execution state for process. Defines properties of a single virtual
%%% thread: registers, instruction pointer, stack, thread context and process
%%% dictionary
%%% @end
%%%-------------------------------------------------------------------
-module(emu_proc).

%% API
-export([new/0]).

-define(NUM_REGS, 32).

-record(proc, { module :: atom()
              , ip = 0 :: non_neg_integer()
              , registers = array:new(?NUM_REGS)
              , stack = []
              }).

new() -> #proc{}.

