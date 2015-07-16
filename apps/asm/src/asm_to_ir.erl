%%%-------------------------------------------------------------------
%%% @doc Parses S file and builds IR file with Gluon instructions
%%% @end
%%%-------------------------------------------------------------------
-module(asm_to_ir).

%% API
-export([process/1]).

-include("../../emuemu/src/emu.hrl").

-record(compile_state, { ir = []
                       , bin = []
                       , mod
                       }).

%% @doc Loads beam S assembly and runs compile on it
process(Fname) ->
  {ok, Asm} = file:consult(Fname),

  %% Cut away header tags
  Trimmed = lists:dropwhile(fun not_func_header/1, Asm),
  Funs = split_funs(Trimmed, []),
  io:format("Funs ~p~n", [Funs]),

  Mod0 = asm_module:new(init),
  Mod1 = asm_module:set_exports(proplists:get_value(exports, Asm), Mod0),

  CompileState = #compile_state{mod = Mod1},
  #compile_state{ir=Code, mod=Mod2}
    = lists:foldr(fun compile_fun_gleam/2, CompileState, Funs),
  Code1 = lists:flatten(Code),
  Mod3 = asm_module:set_ir(Code1, Mod2),

  {Bin0, Mod4} = lists:foldr(fun compile_irop/2, {[], Mod3}, Code1),
  Bin  = iolist_to_binary(Bin0),
  Mod  = asm_module:set_bin(Bin, Mod4),

  io:format("Module ~p~n", [Mod]),
  %ok = asm_module:write_ir(Fname ++ ".ir", Mod),
  ok = file:write_file(Fname ++ ".gleam", asm_module:to_binary(Mod)).

%% @doc Predicate to separate fun headers
not_func_header({function, _, _, _}) -> false;
not_func_header(_) -> true.

%% @doc Splits funs using {function,_,_,_} as first line in each block.
%% Takes Asm with stripped headers; Returns [{fun_name, arity, [code]}]
split_funs([], Accum) -> lists:reverse(Accum);
split_funs([{function, FName, FArity, _} | Code], Accum) ->
  {Fun, Remaining} = lists:splitwith(fun not_func_header/1, Code),
  split_funs(Remaining, [{FName, FArity, Fun} | Accum]).

compile_fun_gleam({F, Arity, Code}, CState) ->
  CState1 = lists:foldl(fun gleam_op/2, CState, Code),
  %% First opcode in function is always label, get it
  {label, FLabel} = hd(Code),
  M1 = CState1#compile_state.mod,
  {FunAtomIndex, M2} = asm_module:find_or_create_atom(F, M1),
  M3 = asm_module:add_fun(FunAtomIndex, Arity, FLabel, M2),
  CState1#compile_state{mod=M3}.

%gleam_op({line, _}, #compile_state{}=CState) -> CState;
%gleam_op({label, _}, #compile_state{}=CState) -> CState;
gleam_op(X, #compile_state{}=CState) when is_atom(X) -> gleam_op({X}, CState);
gleam_op({func_info, _M, _F, _A}, #compile_state{}=CState) -> CState;
gleam_op({gc_bif, Name, Fail, Bif, Args, Result}, #compile_state{}=CState) ->
  Opcode = case length(Args) of
             1 -> gc_bif1;
             2 -> gc_bif2;
             3 -> gc_bif3
           end,
  Op1 = list_to_tuple([Opcode, Name, Fail, Bif] ++ Args ++ [Result]),
  op(Op1, CState);
gleam_op({test, Test, Label, Args}, #compile_state{}=CState) ->
  Args1 = case is_list(Args) of true -> Args; _ -> [Args] end,
  Op1 = list_to_tuple([Test, Label] ++ Args1),
  op(Op1, CState);
gleam_op(Src, #compile_state{}=CState) ->
  op(Src, CState).

op(Src, #compile_state{}=CState) ->
  io:format("gleam_op src=~p~n", [Src]),
  Op = element(1, Src),
  %Opcode = asm_genop:opcode(Op),
  Arity = asm_genop:arity(Op),
  Args = [element(ArgIndex+1, Src) || ArgIndex <- lists:seq(1, Arity)],
  emit_gleam_op({Op, Args}, CState).

emit_gleam_op(Op, CState) when not is_list(Op) -> emit_gleam_op([Op], CState);
emit_gleam_op(Ops, CState = #compile_state{ir = Ir0}) ->
  %% TODO: ++ is O(N^2)
  CState#compile_state{ ir = Ir0 ++ Ops }.

%% Catch label instruction and write its offset in bytes to separate map
compile_irop({label, [N]}, {Accum, Mod0}) ->
  Offset = iolist_size(Accum),
  %io:format("label ~p pos ~p~n", [N, Offset]),
  {Accum, asm_module:register_label(N, Offset, Mod0)};
compile_irop({Op, OpArgs}, {Accum, Mod0}) ->
  CompileArg = fun(Arg, {Accum_, M}) ->
                 {BinArg, M1} = asm_irop:encode_arg(Arg, M),
                 {[BinArg | Accum_], M1}
               end,
  {Args, Mod} = lists:foldl(CompileArg, {[], Mod0}, OpArgs),
  Compiled = [asm_genop:opcode(Op) | Args],
  {[Compiled | Accum], Mod}.

%% compile_fun_ir({F, Arity, Code}, CState) ->
%%   CState1 = lists:foldl(fun c_op/2, CState, Code),
%%   %% First opcode in function is always label, get it
%%   {label, FLabel} = hd(Code),
%%   M1 = CState1#compile_state.mod,
%%   {FunAtomIndex, M2} = asm_module:find_or_create_atom(F, M1),
%%   M3 = asm_module:add_fun(FunAtomIndex, Arity, FLabel, M2),
%%   CState1#compile_state{mod=M3}.

%% %% @doc Compiles individual opcodes to Gluon Intermediate
%% %% CState = compiler state, namely function base offset for labels
%% c_op(BeamSrc={label, L}, #compile_state{}=CState) ->
%%   %%Mod1 = asm_module:register_label(L, length(Acc), Mod0),
%%   %%CState#compile_state{mod=Mod1};
%%   emit(asm_irop:'LABEL'(L), BeamSrc, CState);
%% c_op({func_info, _A1, _A2, _N}, CState=#compile_state{}) -> CState; % NO OP
%% c_op(BeamSrc={line, Props}, CState=#compile_state{mod=Mod0}) ->
%%   case asm_module:get_option(line_numbers, Mod0) of
%%      true -> case lists:keyfind(location, 1, Props) of
%%                false -> CState;
%%                {location, F, L} ->
%%                  {Op, Mod1} = asm_irop:'LINE'(F, L, Mod0),
%%                  emit(Op, BeamSrc, CState#compile_state{mod=Mod1})
%%              end;
%%      false -> CState
%%   end;
%% c_op(BeamS={move, Src, Dst}, CState = #compile_state{mod=Mod0}) ->
%%   {MoveOp, Mod1} = asm_irop:'MOVE'(Src, Dst, Mod0),
%%   emit(MoveOp, BeamS, CState#compile_state{mod=Mod1});
%% c_op(BeamSrc={gc_bif, Lbl, _OnFail, _Bif, Args, ResultDst}, CState=#compile_state{}) ->
%%   emit_bif_call(Lbl, Args, ResultDst, BeamSrc, CState);
%% c_op(BeamSrc={call_only, Arity, Label}, CState=#compile_state{mod=Mod0}) ->
%%   %% Do a tail recursive call to the function at Label. Do not update the CP reg.
%%   {CallOp, Mod1} = asm_irop:'TAILCALL'(Arity, Label, Mod0),
%%   emit(CallOp, BeamSrc, CState#compile_state{mod=Mod1});
%% c_op(BeamSrc={call_ext, Arity, Label}, CState=#compile_state{mod=Mod0}) ->
%%   %% Call the function of arity Arity pointed to by Destination.
%%   %% Save the next instruction as the return address in the CP register.
%%   {CallOp, Mod1} = asm_irop:'CALL'(Label, Arity, Mod0),
%%   emit(CallOp, BeamSrc, CState#compile_state{mod=Mod1});
%% c_op(BeamSrc={call_ext_only, Arity, Label}, CState=#compile_state{mod=Mod0}) ->
%%   %% Do a tail recursive call to the function at Label. Do not update the CP reg.
%%   {CallOp, Mod1} = asm_irop:'TAILCALL'(Arity, Label, Mod0),
%%   emit(CallOp, BeamSrc, CState#compile_state{mod=Mod1});
%% c_op(BeamS={test, Test, Label, Args}, CState=#compile_state{mod=Mod0}) ->
%%   {TestOp, Mod1} = asm_irop:'TEST'(Test, Label, Args, Mod0),
%%   emit(TestOp, BeamS, CState#compile_state{mod=Mod1});
%% c_op(BeamSrc= return, CState=#compile_state{}) ->
%%   emit(asm_irop:'RET'(), BeamSrc, CState);
%% c_op(BeamSrc={get_list, Src, OutHead, OutTail}, CState=#compile_state{mod=Mod0}) ->
%%   %%emit_bif_call(gluon_hd_tl, [Src], [ref(Head), ref(Tail)], BeamSrc, CState);
%%   {Op, Mod1} = asm_irop:'GET_LIST'(Src, OutHead, OutTail, Mod0),
%%   emit(Op, BeamSrc, CState#compile_state{mod=Mod1});
%% c_op(BeamSrc={badmatch, Value}, CState=#compile_state{}) ->
%%   emit_bif_call(error, [{atom, badmatch}, Value], ?nil, BeamSrc, CState);
%% c_op(BeamS={allocate, StackNeed, Live}, CState=#compile_state{}) ->
%%   emit_bif_call(gluon_allocate, [{integer, StackNeed}, {integer, Live}]
%%                , ?nil, BeamS, CState);
%%   % TODO asm opcode maybe: emit(asm_op:'ALLOCATE'(StackNeed, Live), CState);
%% c_op(BeamSrc={deallocate, N}, CState=#compile_state{}) ->
%%   emit_bif_call(gluon_deallocate, [{integer, N}], ?nil, BeamSrc, CState);
%% c_op(UnkOp, {_Acc, _MState, _CState}) ->
%%   erlang:error({unknown_op, UnkOp}).
%%   %{[{unknown, UnkOp} | Acc], MState, CState}.
%%
%% emit_bif_call(Lbl, Args, ResultDst, BeamSrc, CState=#compile_state{mod=Mod0})->
%%   {SetArgs, _, Mod1} = lists:foldl(fun fold_set_arg/2, {[], 0, Mod0}, Args),
%%   SetArgs1 = lists:reverse(SetArgs),
%%   {CallOp, Mod2} = asm_irop:'CALL'(Lbl, length(Args), bif, ResultDst, Mod1),
%%   emit(SetArgs1 ++ [CallOp], BeamSrc, CState#compile_state{mod=Mod2}).
%%
%% fold_set_arg(Arg, {Acc, Reg, MState}) ->
%%   %{Op, MState1} = asm_irop:'PUSH'(Arg, MState),
%%   {Op, MState1} = asm_irop:'MOVE'(Arg, {x, Reg}, MState),
%%   {[Op | Acc], Reg+1, MState1}.

%% emit(Op, CState) when not is_list(Op) -> emit([Op], CState);
%% emit(Ops, CState = #compile_state{accum = Acc0}) ->
%%   %% TODO: ++ is O(N^2)
%%   CState#compile_state{accum = Acc0 ++ Ops}.

%% emit(nop, _, State) -> State;
%% emit(Op, BeamSrc, CState = #compile_state{accum = Acc0}) when not is_list(Op) ->
%%   Op1 = maps:put(src, BeamSrc, Op),
%%   %% TODO: ++ is O(N^2)
%%   CState#compile_state{accum = Acc0 ++ [Op1]};
%% emit(Ops0, BeamSrc, CState = #compile_state{accum = Acc0}) ->
%%   Ops1 = lists:filter(fun(X) -> X =/= nop end, Ops0),
%%   Ops2 = #{src => BeamSrc, irops => Ops1},
%%   %% TODO: ++ is O(N^2)
%%   CState#compile_state{accum = Acc0 ++ [Ops2]}.
%%
