%%%-------------------------------------------------------------------
%% @doc emuemu public API
%% @end
%%%-------------------------------------------------------------------

-module(emu_app).

-behaviour(application).

%% Application callbacks
-export([start/2
        , stop/1, start/0]).

%%====================================================================
%% API
%%====================================================================

start() ->
  simulate().

start(_StartType, _StartArgs) ->
  emu_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
simulate() ->
  {ok, VM} = emu_machine:start_link(),
  emu_machine:load_module(VM, init, "test/init.S.ir"),
  {ok, _MainThread} = emu_machine:spawn(init, test, []).
