%%%-------------------------------------------------------------------
%%% @doc Defines properties of virtual machine (CPU, memory, resources)
%%% @end
%%%-------------------------------------------------------------------
-module(emu_machine).

%% API
-export([]).

-define(NUM_REGS, 32).
-record(machine, { modules = orddict:new()
                 , registers = array:new(?NUM_REGS)
                 }).
