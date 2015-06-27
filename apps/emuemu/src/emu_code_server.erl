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
-export([start_link/0
        , add_code/2
        , find_mfa/2
        , fetch/2
        , step/2
        , label_to_offset/3
        , jump/2
        , get_module/1
        , find_atom/3
        , get_literal/3
        ]).

-export_type([code_pointer/0]).

-record(code_server, { modules = orddict:new()
                     , code = ets:new(?MODULE, [ordered_set]) :: ets:tab()
                     }).

-record(code_pointer, { code_server :: pid()
                      , offset :: non_neg_integer()
                      , module :: atom()
                      }).
-type code_pointer() :: #code_pointer{}.

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

%% @doc Starts the server
-spec start_link() -> {ok, pid()} | ignore | {error, Error :: any()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Registers loaded asm_module in Gluon IR format (erlang tuples)
add_code(CodeSrv, AsmM) when is_pid(CodeSrv) ->
  gen_server:call(CodeSrv, {add_code, AsmM}).

%% @doc Returns opaque code pointer which points to beginning of function
find_mfa(CodeSrv, {M, F, Arity}) when is_pid(CodeSrv) ->
  gen_server:call(CodeSrv, {find_mfa, {M, F, Arity}}).

fetch(CodeSrv, IP) when is_pid(CodeSrv) ->
  gen_server:call(CodeSrv, {fetch, IP}).

find_atom(CodeSrv, Module, AtomIndex) when is_pid(CodeSrv) ->
  gen_server:call(CodeSrv, {find_atom, Module, AtomIndex}).

get_literal(CodeSrv, Module, LitIndex) when is_pid(CodeSrv) ->
  gen_server:call(CodeSrv, {get_literal, Module, LitIndex}).

step(N, #code_pointer{offset=Offset}=IP) ->
  IP#code_pointer{offset=Offset+N}.

jump(N, #code_pointer{}=IP) ->
  IP#code_pointer{offset=N}.

get_module(#code_pointer{module=M}) -> M.

label_to_offset(CodeSrv, Mod, Label) when is_pid(CodeSrv), is_atom(Mod) ->
  gen_server:call(CodeSrv, {label_to_offset, Mod, Label}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) -> {ok, #code_server{}}.

handle_call({add_code, AsmM}, _From
           , State=#code_server{ code=Ets
                               , modules=Modules}) ->
  IR      = asm_module:get_ir(AsmM),
  Modname = asm_module:get_name(AsmM),
  StoreFun = fun
    (X, Offset) when element(1,X) =:= '//' -> Offset; % Skip comment irops
    (IROp, Offset) ->
      %% Use {Modname, Offset} as key for opcodes.
      ets:insert(Ets, {{Modname, Offset}, IROp}),
      Offset + 1
  end,
  lists:foldl(StoreFun, 0, IR),
  Modules1 = orddict:store(Modname, AsmM, Modules),
  {reply, ok, State#code_server{modules = Modules1}};
handle_call({find_mfa, {M, F, Arity}}, _From, State) ->
  {reply, find_mfa_i({M, F, Arity}, State), State};
handle_call({fetch, IP}, _From, State=#code_server{code=Ets}) ->
  [{_Key, Instr}] = ets:lookup(Ets, {IP#code_pointer.module, IP#code_pointer.offset}),
  io:format("fetch -> ~p~n", [Instr]),
  {reply, {ok, Instr}, State};
handle_call({label_to_offset, Mod, Label}, _From, State) ->
  AsmMod = orddict:fetch(Mod, State#code_server.modules),
  {ok, Offset} = asm_module:label_to_offset(AsmMod, Label),
  {reply, {ok, Offset}, State};
handle_call({find_atom, Module, AtomIndex}, _From, State) ->
  {reply, find_atom_i(Module, AtomIndex, State), State};
handle_call({get_literal, Module, LIndex}, _From, State) ->
  {reply, get_literal_i(Module, LIndex, State), State};
handle_call(Request, _From, State) ->
  {reply, {?MODULE, bad_request, Request}, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

find_atom_i(Module, AtomIndex, #code_server{modules=Modules}) ->
  AsmM = orddict:fetch(Module, Modules),
  asm_module:find_atom(AtomIndex, AsmM).

get_literal_i(Module, LitIndex, #code_server{modules=Modules}) ->
  AsmM = orddict:fetch(Module, Modules),
  asm_module:find_literal(LitIndex, AsmM).

find_mfa_i({M, F, Arity}, #code_server{modules=Modules}) ->
  AsmM = orddict:fetch(M, Modules),
  Ok_MLabel = asm_module:funarity_to_label(AsmM, {F, Arity}),
  Ok_Offset = asm_module:label_to_offset(AsmM, Ok_MLabel),
  make_ptr(M, Ok_Offset).

make_ptr(Mod, {ok, Offs}) ->
  {ok, #code_pointer{module=Mod, code_server=self(), offset=Offs}};
make_ptr(_M, {error, _}=E) -> E.
