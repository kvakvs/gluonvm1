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

-record(proc, { vm :: pid()
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
  {ok, Instr} = fetch(State),
  State1 = execute_op(Instr, State),
  ?MODULE:tick(self()),
  {noreply, State1}.

%% @doc Calls code server to get next instruction
-spec fetch(#proc{}) -> {emu_code_server:code_pointer(), #proc{}}.
fetch(#proc{code_server=CodeSvr, ip=IP}) ->
  emu_code_server:fetch(CodeSvr, IP).

%% @doc Advances pointer by N instructions forward
step(N, State=#proc{ip=IP}) ->
  State#proc{ip=emu_code_server:step(N, IP)}.

%% @doc Resets pointer to a new location (local or far jump)
jump({'$LABEL', L}, State=#proc{ip=IP, code_server=CodeSrv}) ->
  {ok, N} = emu_code_server:label_to_offset(CodeSrv, current_module(State), L),
  io:format("action: jump to label ~p (offset ~p)~n", [L, N]),
  State#proc{ip=emu_code_server:jump(N, IP)}.

%% @doc Calculates value based on its tag ($IMM, $REG, $STACK... etc)
evaluate({'$IMM', X}, _State) -> {ok, X};
evaluate({'$REG', R}, #proc{registers=Regs}) ->
  {ok, array:get(R, Regs)}.

%% @doc Puts a value (untagged by evaluate) to some destination
move(Value, {'$REG', R}, State=#proc{registers=Regs}) ->
  io:format("action: move ~p to register ~p~n", [Value, R]),
  Regs1 = array:set(R, Value, Regs),
  {ok, State#proc{registers=Regs1}}.

test_op(is_lt, [A0, B0], State) ->
  {ok, A} = evaluate(A0, State),
  {ok, B} = evaluate(B0, State),
  A < B.

execute_op({'LINE', _FileLiteral, _Line}, State) -> step(1, State);
execute_op({'MOVE', TaggedValue, Dst}, State) ->
  {ok, Value} = evaluate(TaggedValue, State),
  {ok, State1} = move(Value, Dst, State),
  step(1, State1);
execute_op({'TAILCALL', _Live, Dst}, State) ->
  jump(Dst, State);
execute_op({'TEST', TestOp, Label, Args}, State) ->
  TestResult = test_op(TestOp, Args, State),
  io:format("action: test ~s for ~p = ~p~n", [TestOp, Args, TestResult]),
  case TestResult of
    true  -> step(1, State);
    false -> jump(Label, State)
  end;
execute_op({'PUSH', TaggedValue}, State=#proc{stack=Stack}) ->
  {ok, Value} = evaluate(TaggedValue, State),
  step(1, State#proc{stack=[Value | Stack]});
execute_op(Instr, _State) ->
  io:format("~s unknown instr ~p~n", [?MODULE_STRING, Instr]),
  erlang:throw('BAD_INSTR').

current_module(#proc{ip=IP}) -> emu_code_server:get_module(IP).
