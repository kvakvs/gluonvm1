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
-include("emu.hrl").

-record(proc, { vm :: pid()
              , code_server :: pid()
              , ip = ?nil :: emu_code_server:code_pointer()
              , cp = ?nil :: emu_code_server:code_pointer()
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
jump({{'$ATOM', FunAtomIndex}, Arity, non_bif}, State=#proc{code_server=CodeSrv}) ->
  %% Local jump to function
  M = current_module(State),
  {ok, FunAtom} = emu_code_server:find_atom(CodeSrv, M, FunAtomIndex),
  {ok, IP} = emu_code_server:find_mfa(CodeSrv, {M, FunAtom, Arity}),
  io:format("action: jump to fun ~p/~p (~p)~n", [FunAtom, Arity, IP]),
  State#proc{ip=IP};
jump({'$LABEL', L}, State=#proc{code_server=CodeSrv}) ->
  %% Local jump to label
  {ok, N} = emu_code_server:label_to_offset(CodeSrv, current_module(State), L),
  io:format("action: jump to label ~p (offset ~p)~n", [L, N]),
  jump(N, State);
jump(N, State=#proc{ip=IP}) when is_integer(N) ->
  %% Local jump to numeric offset
  State#proc{ip=emu_code_server:jump(N, IP)}.

%% @doc Calculates value based on its tag ($IMM, $REG, $STACK... etc)
evaluate(X, _State) when is_atom(X) -> {ok, X}; % do not evaluate
evaluate({'$REF', X}, _State) -> {ok, X}; % do not evaluate, unwrap ref
evaluate({'$IMM', X}, _State) -> {ok, X};
evaluate({'$REG', R}, #proc{registers=Regs}) ->
  {ok, array:get(R, Regs)};
evaluate({'$LIT', L}, State=#proc{code_server=CodeSrv}) ->
  emu_code_server:get_literal(CodeSrv, current_module(State), L).

%% @doc Puts a value (untagged by evaluate) to some destination
move(?nil, _, State) -> {ok, State}; % void does not go anywhere
move(Value, {'$REG', R}, State=#proc{registers=Regs}) ->
  Regs1 = array:set(R, Value, Regs),
  io:format("action: move ~p to R~p~n", [Value, R]),
  dbg_regs(Regs1),
  {ok, State#proc{registers=Regs1}}.

dbg_regs(R) ->
  [io:format("~s=~p; "
            , [list_to_atom([$R, $0+I])
              , array:get(I, R)]) || I <- lists:seq(0, 9)],
  io:format("~n", []).

test_op(is_lt, [A0, B0], State) ->
  {ok, A} = evaluate(A0, State),
  {ok, B} = evaluate(B0, State),
  A < B;
test_op(is_nonempty_list, [L0], State) ->
  {ok, L} = evaluate(L0, State),
  is_list(L) andalso length(L) > 0;
test_op(is_nil, [N0], State) ->
  {ok, N} = evaluate(N0, State),
  N =:= ?nil.

push(Value, State = #proc{stack=Stack}) ->
  io:format("action: push ~p | ~p~n", [Value, Stack]),
  State#proc{stack=[Value | Stack]}.

-spec pop(#proc{}) -> {ok, any(), #proc{}}.
pop(#proc{stack=[]}) ->
  erlang:error(stack_underflow);
pop(State = #proc{stack=[Value|Stack]}) ->
  io:format("action: pop value ~p, remaining: ~p~n", [Value, Stack]),
  {ok, Value, State#proc{stack=Stack}}.

call_bif({FunAtom, Arity, bif}, ResultDst
        , State=#proc{}) ->
  %% Calling a builtin
  %M = current_module(State),
  %{ok, FunAtom} = emu_code_server:find_atom(CodeSrv, M, FunAtomIndex),
  %% Pop args for builtin
  {Args, State1} = fetch_n_args(Arity, State),
  io:format("action: bif ~p/~p args ~p~n", [FunAtom, Arity, Args]),
  {Result, State2} = find_and_call_bif(FunAtom, Args, State1),
  {ok, State3} = move(Result, ResultDst, State2),
  State3.

fetch_n_args(N, State) ->
  %% {Args, State1} =
  Args = lists:foldl(fun(Seq, A) ->
                %{ok, Arg, St1} = pop(St),
                {ok, Arg} = evaluate({'$REG', Seq-1}, State),
                [Arg | A]
              end, [], lists:seq(1, N)),
  {Args, State}.

find_and_call_bif(gluon_hd_tl, Args, PState) -> g_hd_tl(Args, PState);
find_and_call_bif(gluon_allocate, Args, PState) -> g_allocate(Args, PState);
find_and_call_bif(gluon_deallocate, Args, PState) -> g_deallocate(Args, PState);
find_and_call_bif(F, Args, PState) -> {apply(erlang, F, Args), PState}.

set_cp(CP, State=#proc{}) -> State#proc{cp = CP}.

g_hd_tl([Src, Hd, Tl], PState=#proc{}) ->
  %% Get the head and tail (or car and cdr) parts of a list (a cons cell) from
  %% Source and put them into the registers Head and Tail.
  {ok, PState1} = move(hd(Src), Hd, PState),
  {ok, PState2} = move(tl(Src), Tl, PState1),
  {?nil, PState2}.

g_allocate([StackNeed, _Live], PState=#proc{}) ->
  %% Allocate space for StackNeed words on the stack. If a GC is needed
  %% during allocation there are Live number of live X registers. Also save the
  %% continuation pointer (CP) on the stack.
  PState1 = lists:foldl( fun(_, Proc) -> push(?nil, Proc) end
                       , PState
                       , lists:seq(1, StackNeed)),
  PState2 = push(PState1#proc.cp, PState1),
  {?nil, PState2#proc{cp = undefined}}.

g_deallocate([N], PState=#proc{}) ->
  %% Restore the continuation pointer (CP) from the stack and deallocate
  %% N+1 words from the stack (the + 1 is for the CP).
  PState1 = PState#proc{cp = pop(PState)},
  PState2 = lists:foldl( fun(_, Proc) -> pop(Proc) end
                       , PState1
                       , lists:seq(1, N)),
  {?nil, PState2}.

  execute_op({'LINE', _FileLiteral, _Line}, State) -> step(1, State);
execute_op({'MOVE', TaggedValue, Dst}, State) ->
  {ok, Value} = evaluate(TaggedValue, State),
  {ok, State1} = move(Value, Dst, State),
  step(1, State1);
execute_op({'TAILCALL', _Live, Dst}, State) ->
  jump(Dst, State);
execute_op({'CALL', Dst, Arity, IsBif, ResultDst}, State = #proc{ip=IP}) ->
  %% save next instruction after current and jump (or not save for bif)
  case IsBif of
    bif     -> step(1, call_bif({Dst, Arity, IsBif}, ResultDst, State));
    non_bif -> set_cp(emu_code_server:step(1, IP), State)
  end;
execute_op({'CALL', IrMfa, _Arity}, State = #proc{ip=IP}) ->
  %% Check if this is our module or library
  {'$MFA', M, F, Arity} = IrMfa,
  %CurrentM = current_module(State),
  case find_mfa({M, F, Arity}, State) of
    {error, _} ->
      {Args, State1} = fetch_n_args(Arity, State),
      io:format("action: call host ~p:~p/~p args ~p~n", [M, F, Arity, Args]),
      State2 = move(apply(M, F, Args), {'$REG', 0}, State1),
      step(1, State2);
    {ok, IP}   ->
      State1 = set_cp(emu_code_server:step(1, IP), State),
      jump(IP, State1)
  end;
execute_op({'TEST', TestOp, Label, Args}, State) ->
  TestResult = test_op(TestOp, Args, State),
  io:format("action: test ~s for ~p = ~p~n", [TestOp, Args, TestResult]),
  case TestResult of
    true  -> step(1, State);
    false -> jump(Label, State)
  end;
execute_op({'PUSH', TaggedValue}, State=#proc{}) ->
  {ok, Value} = evaluate(TaggedValue, State),
  step(1, push(Value, State));
execute_op({'RET'}, #proc{cp=undefined}=State) ->
  {ok, R0} = evaluate({'$REG', 0}, State),
  io:format("action: return to undefined, R0=~p, end program~n", [R0]),
  erlang:error('PROGRAM_END');
execute_op({'RET'}, State=#proc{cp=CP}) ->
  io:format("action: return to ~p~n", [CP]),
  set_cp(undefined, State#proc{ip=CP});
execute_op(Instr, _State) ->
  io:format("~s unknown instr ~p~n", [?MODULE_STRING, Instr]),
  erlang:throw('BAD_INSTR').

current_module(#proc{ip=IP}) -> emu_code_server:get_module(IP).

%% Returns {ok, IP} or {error, _}
find_mfa({M, F, Arity}, #proc{code_server = CodeSrv}) ->
  code:ensure_loaded(M),
  case erlang:function_exported(M, F, Arity) of
    true -> {error, exists_in_host_system};
    false -> % exists in emuemu
      emu_code_server:find_mfa(CodeSrv, {M, F, Arity})
  end.
