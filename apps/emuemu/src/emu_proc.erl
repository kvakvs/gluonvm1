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
-export([ start_link/1
        , call/4]).

-define(NUM_REGS, 32).

-record(proc, { module :: atom()
              , vm :: pid()
              , ip = {undefined, 0} :: {atom(), non_neg_integer()}
              , registers = array:new(?NUM_REGS)
              , stack = []
              }).


-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

%% @doc Starts the server
-spec start_link(pid()) -> {ok, pid()} | ignore | {error, Error :: any()}.
start_link(VM) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [VM], []).

call(VM, M, F, Args) ->
  gen_server:call(VM, {call, M, F, Args}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([VM]) -> {ok, #proc{vm=VM}}.

handle_call({call, M, F, Args}, _From, State=#proc{vm=VM}) ->
  State1 = State#proc{ip = emu_machine:code_find_mfa(VM, M, F, length(Args))},
  {reply, ok, State1};
handle_call(Request, _From, State) ->
  {reply, {?MODULE, bad_request, Request}, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
