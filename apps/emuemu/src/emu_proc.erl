%%%-------------------------------------------------------------------
%%% @doc Execution state for process. Defines properties of a single virtual
%%% thread: registers, instruction pointer, stack, thread context and process
%%% dictionary
%%% @end
%%%-------------------------------------------------------------------
-module(emu_proc).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2
        , terminate/2, code_change/3]).

%% API
-export([start_link/2
        , call/4, tick/1]).

-define(NUM_REGS, 32).

-record(proc, { module :: atom()
              , vm :: pid()
              , code_server :: pid()
              , ip = {undefined, 0} :: {atom(), non_neg_integer()}
              , registers = array:new(?NUM_REGS)
              , stack = []
              , heap = orddict:new()
              }).


-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

%% @doc Starts the server
-spec start_link(pid(), pid()) -> {ok, pid()} | ignore | {error, Error :: any()}.
start_link(VM, CodeSrv) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [VM, CodeSrv], []).

call(Proc, M, F, Args) ->
  gen_server:call(Proc, {call, M, F, Args}).

%% @doc async: Starts ticking clock for process, it will self-sustain by sending
%% another tick to self() after execution step is done
tick(Proc) ->
  gen_server:cast(Proc, tick).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([VM, CodeSrv]) -> {ok, #proc{vm=VM, code_server=CodeSrv}}.

handle_call({call, M, F, Args}, _From
           , State=#proc{ stack=Stack
                        , code_server=CodeSrv
                        , ip=IP0
                        }) ->
  {ok, IP} = emu_code_server:find_mfa(CodeSrv, {M, F, length(Args)}),
  State1 = State#proc{stack=[IP0 | Stack], ip=IP},
  io:format("proc call ~p~n", [dbg_state(State1)]),
  {reply, ok, State1};
handle_call(Request, _From, State) ->
  {reply, {?MODULE, bad_request, Request}, State}.

handle_cast(tick, State) -> tick_i(State);
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
dbg_state(#proc{}=P) ->
  lists:zip(record_info(fields, proc), tl(tuple_to_list(P))).

%% @doc Fetch an instruction and do a step
-spec tick_i(#proc{}) -> {noreply, #proc{}}.
tick_i(State = #proc{}) ->
  Instr = fetch_next(State),
  State1 = execute_op(Instr, State),
  {noreply, State1}.

%% @doc Calls code server to get next instruction
-spec fetch_next(#proc{}) -> {emu_code_server:code_pointer(), #proc{}}.
fetch_next(State=#proc{code_server=CodeSvr, ip=IP}) ->
  {IP1, Instr} = emu_code_server:fetch_next(CodeSvr, IP),
  {Instr, State#proc{ip=IP1}}.

execute_op(Instr, State) ->
  io:format("~s unknown instr ~p~n", [?MODULE_STRING, Instr]),
  State.
