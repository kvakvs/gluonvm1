%%%-------------------------------------------------------------------
%% @doc prototype beam runner public API
%% @end
%%%-------------------------------------------------------------------

-module(prototype_app).

-behaviour(application).

%% Application callbacks
-export([ start/2
        , stop/1
        , start/0
        ]).

%%====================================================================
%% API
%%====================================================================

start() ->
%%   prototype_emu:run("test/g_test1.beam", test2, []).

%%  prototype_emu:run("test/g_test1.beam", f_test, []).

   J1 = {struct, [{"hello", "world"}, {"test", 123}]},
   prototype_emu:run("test/mochijson.beam", encode, [J1]).

start(_StartType, _StartArgs) ->
  ok.
  %'asm_sup':start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
