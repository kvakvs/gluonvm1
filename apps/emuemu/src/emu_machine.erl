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

-record(machine, { pid_counter = 0
                 , processes = orddict:new()
                 , code_server :: pid()
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

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
  {ok, CodeSrv} = emu_code_server:start_link(),
  {ok, #machine{code_server=CodeSrv}}.

handle_call({load_module, _Name, Filename}, _From
           , State) ->
  Mod = asm_module:read_ir(Filename),
  emu_code_server:add_code(State#machine.code_server, Mod),
  {reply, ok, State};
handle_call({spawn, M, F, Args}, _From
           , State=#machine{ pid_counter=Counter
                           , processes=Procs
                           , code_server=CodeSrv}) ->
  Counter1 = Counter + 1,
  {ok, Proc} = emu_proc:start_link(self(), CodeSrv),
  emu_proc:call(Proc, M, F, Args),
  emu_proc:tick(Proc), % start the execution (ticking clock)
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
