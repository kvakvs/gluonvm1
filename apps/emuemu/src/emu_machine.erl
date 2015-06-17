%%%-------------------------------------------------------------------
%%% @doc Defines properties of virtual machine (modules, atoms, processes and
%%% other resources)
%%% @end
%%%-------------------------------------------------------------------
-module().

%% API
-export([ new/0
        , load_module/3]).

-record(machine, { modules = orddict:new()
                 , processes = orddict:new()
                 }).


new() -> #machine{}.

-module(emu_machine).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-record(state, {}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Starts the server
-spec start_link() -> {ok, pid()} | ignore | {error, Error :: any()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) -> {ok, #state{}}.

handle_call({load_module, Name, Filename}, _From
           , State=#machine{modules=Modules}) ->
  M = asm_module:read_ir(Filename),
  Modules1 = orddict:store(Name, M, Modules),
  {reply, ok, State#machine{modules=Modules1}};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
