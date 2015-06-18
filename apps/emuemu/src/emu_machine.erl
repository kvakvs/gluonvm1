%%%-------------------------------------------------------------------
%%% @doc Defines properties of virtual machine (modules, atoms, processes and
%%% other resources)
%%% @end
%%%-------------------------------------------------------------------
-module(emu_machine).

-behaviour(gen_server).

%% gen_server callbacks
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2
        , terminate/2, code_change/3]).

%% API
-export([load_module/3
        , start_link/0
        , spawn/4
        ]).

-define(SERVER, ?MODULE).

-record(machine, { modules = orddict:new()
                 , pid_counter = 0
                 , processes = orddict:new()
                 , code_tab = ets:new(code, [ordered_set])
                 }).

%%====================================================================
%% API
%%====================================================================

%% @doc Starts the server
-spec start_link() -> {ok, pid()} | ignore | {error, Error :: any()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

load_module(VM, Name, Filename) ->
  gen_server:call(VM, {load_module, Name, Filename}).

spawn(VM, M, F, Args) ->
  gen_server:call(VM, {spawn, M, F, Args}).

code_find_mfa(VM, M, F, Arity) ->
  gen_server:call(VM, {code_find_mfa, M, F, Arity}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) -> {ok, #machine{}}.

handle_call({load_module, Name, Filename}, _From
           , State=#machine{modules=Modules}) ->
  M = asm_module:read_ir(Filename),
  Modules1 = orddict:store(Name, M, Modules),
  store_code(M, State),
  {reply, ok, State#machine{modules=Modules1}};
handle_call({spawn, M, F, Args}, _From
           , State=#machine{ pid_counter=Counter
                           , processes=Procs}) ->
  Counter1 = Counter + 1,
  {ok, Proc} = emu_proc:start_link(self()),
  %% FIXME TODO: This will deadlock while call requests back VM for code address
  emu_proc:call(Proc, M, F, Args),
  Procs1 = orddict:store(Counter1, Proc, Procs),
  {reply, {ok, Counter1}, State#machine{ pid_counter=Counter1
                                       , processes=Procs1}};
handle_call(Request, _From, State) ->
  {reply, {?MODULE, bad_request, Request}, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
store_code(Mod, #machine{code_tab=CodeT}) ->
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
