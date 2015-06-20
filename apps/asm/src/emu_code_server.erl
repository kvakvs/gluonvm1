%%%-------------------------------------------------------------------
%%% @doc Owns ETS table with opcodes, loads, updates and fetches them
%%% @end
%%%-------------------------------------------------------------------
-module(emu_code_server).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2
        , terminate/2, code_change/3]).

%% API
-export([ start_link/0
        ]).

-record(code_server, {code = ets:new(?MODULE, [ordered_set]) :: ets:tab()
                     }).


-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

%% @doc Starts the server
-spec start_link() -> {ok, pid()} | ignore | {error, Error :: any()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

store_code(CodeSrv, Mod) ->
  IR = asm_module:get_ir(Mod),
  Modname = asm_module:get_name(Mod),
  %% Use {Modname, Offset} as key for opcodes. Skip comment irops
  StoreFun = fun
    (X, Offset) when element(1,X) =:= '//' -> Offset;
    (IROp, Offset) ->
      Offset1 = Offset + 1,
      ets:insert(CodeT, {{Modname, Offset1}, IROp}),
      Offset1
  end,
  lists:foreach(StoreFun, IR).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) -> {ok, #code_server{}}.

handle_call(Request, _From, State) ->
  {reply, {?MODULE, bad_request, Request}, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
