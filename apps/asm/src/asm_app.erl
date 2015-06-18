%%%-------------------------------------------------------------------
%% @doc asm public API
%% @end
%%%-------------------------------------------------------------------

-module('asm_app').

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
  %application:start(asm)
  process("test/init.S").

start(_StartType, _StartArgs) ->
  'asm_sup':start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Loads beam S assembly and runs compile on it
process(Fname) ->
  {ok, Asm} = file:consult(Fname),

  %% Cut away header tags
  Trimmed = lists:dropwhile(fun not_func_header/1, Asm),
  Funs = split_funs(Trimmed, []),
  io:format("Funs ~p~n", [Funs]),

  Mod0 = asm_module:new(init),
  Mod1 = asm_module:set_exports(proplists:get_value(exports, Asm), Mod0),

  CompileFun = fun(F, {Accum, MState}) ->
                {Piece, MState1} = compile_one_fun(F, MState),
                Accum1 = [Piece | Accum],
                {Accum1, MState1}
              end,
  {Code, Mod2} = lists:foldr(CompileFun, {[], Mod1}, Funs),
  Mod = asm_module:set_ir(lists:flatten(Code), Mod2),
  io:format("Module ~p~n", [Mod]),
  ok = asm_module:write_ir(Fname ++ ".ir", Mod).

%% @doc Predicate to separate fun headers
not_func_header({function, _, _, _}) -> false;
not_func_header(_) -> true.

%% @doc Splits funs using {function,_,_,_} as first line in each block.
%% Takes Asm with stripped headers; Returns [{fun_name, arity, [code]}]
split_funs([], Accum) -> lists:reverse(Accum);
split_funs([{function, FName, FArity, _} | Code], Accum) ->
  {Fun, Remaining} = lists:splitwith(fun not_func_header/1, Code),
  split_funs(Remaining, [{FName, FArity, Fun} | Accum]).

compile_one_fun({F, Arity, Code}, MState) ->
  {Ops, MState1} = lists:foldr(fun c_op/2, {[], MState}, Code),
  %% First opcode in function is always label, get it
  {label, FLabel} = hd(Code),
  {FunAtomIndex, MState2} = asm_module:find_or_create_atom(F, MState1),
  MState3 = asm_module:add_fun(FunAtomIndex, Arity, FLabel, MState2),
  {Ops, MState3}.

%% @doc Compiles individual opcodes to Gluon Intermediate
c_op({label, L}, {Acc, MState}) ->
  %%{[asm_op:'LABEL'(L) | Acc], MState}
  MState1 = asm_module:register_label(L, length(Acc), MState),
  {[asm_op:comment([label, L]) | Acc], MState1};
c_op({func_info, _A1, _A2, _N}, {Acc, MState}) -> {Acc, MState}; % NO OP
c_op({line, Props}, {Acc, MState}) ->
  case asm_module:get_option(line_numbers, MState) of
     true -> case lists:keyfind(location, 1, Props) of
               false -> {Acc, MState};
               {location, F, L} ->
                 {Op, MState1} = asm_op:'LINE'(F, L, MState),
                 {[Op | Acc], MState1}
             end;
     false -> {Acc, MState}
  end;
c_op({move, Src, Dst}, {Acc, MState}) ->
  {MoveOp, MState1} = asm_op:'MOVE'(Src, Dst, MState),
  {[MoveOp | Acc], MState1};
c_op({gc_bif, Lbl, _OnFail, Bif, Args, _Reg}, {Acc, MState}) ->
  {CallOp, MState1} = asm_op:'CALL'(Lbl, Bif, MState),
  Acc1 = [CallOp | Acc],
  {Acc2, MState2} = lists:foldr(fun fold_push_argument/2
                               , {Acc1, MState1}
                               , Args),
  {Acc2, MState2};
c_op({call_only, Arity, Label}, {Acc, Module}) ->
  {CallOp, Module1} = asm_op:'TAILCALL'(Arity, Label, Module),
  {[CallOp | Acc], Module1};
c_op({call_ext_only, Arity, Label}, {Acc, Module}) ->
  {CallOp, Module1} = asm_op:'TAILCALL'(Arity, Label, Module),
  {[CallOp | Acc], Module1};
c_op({test, Test, Label, Args}, {Acc, Module}) ->
  {TestOp, Module1} = asm_op:'TEST'(Test, Label, Args, Module),
  {[TestOp | Acc], Module1};
c_op(return, {Acc, Module}) ->
  {[asm_op:'RET'() | Acc], Module};
c_op(UnkOp, {Acc, MState}) -> {[{unknown, UnkOp} | Acc], MState}.

fold_push_argument(Arg, {Acc, MState}) ->
  {Op, MState1} = asm_op:'PUSH'(Arg, MState),
  {[Op | Acc], MState1}.
