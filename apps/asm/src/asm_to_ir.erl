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

  [{module, ModName} | _] = Asm,

    %% Cut away header tags
  Trimmed = lists:dropwhile(fun not_func_header/1, Asm),
  Funs = split_funs(Trimmed, []),
  io:format("Funs ~p~n", [Funs]),

  Mod0 = asm_module:new(ModName),
  Mod1 = asm_module:set_exports(proplists:get_value(exports, Asm), Mod0),

  CompileState = #compile_state{mod = Mod1},
  #compile_state{ir=Code, mod=Mod2}
    = lists:foldr(fun compile_fun_gleam/2, CompileState, Funs),
  Code1 = lists:flatten(Code),
  Mod3 = asm_module:set_ir(Code1, Mod2),

  {Bin0, Mod4} = lists:foldr(fun compile_irop_dbg/2, {[], Mod3}, Code1),
  Bin  = iolist_to_binary(Bin0),
  Mod  = asm_module:set_bin(Bin, Mod4),

  io:format("Module ~p~n", [asm_module:to_proplist(Mod)]),
  %%ok = asm_module:write_ir(Fname ++ ".ir", Mod),
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
  M2 = asm_module:add_fun(F, Arity, FLabel, M1),
  CState1#compile_state{mod=M2}.

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
gleam_op({bif, Lbl, Bif, Args, Result}, #compile_state{}=CState) ->
  Opcode = case length(Args) of
             1 -> bif1;
             2 -> bif2;
             3 -> bif3
           end,
  Op1 = list_to_tuple([Opcode, Lbl, Bif] ++ Args ++ [Result]),
  op(Op1, CState);
gleam_op({test, Test, Label, Args}, #compile_state{}=CState) ->
  Args1 = case is_list(Args) of true -> Args; _ -> [Args] end,
  Op1 = list_to_tuple([Test, Label] ++ Args1),
  op(Op1, CState);
gleam_op(Src, #compile_state{}=CState) ->
  op(Src, CState).

op(Src, #compile_state{}=CState) ->
  %%io:format("gleam_op src=~p~n", [Src]),
  Op = element(1, Src),
  %Opcode = asm_genop:opcode(Op),
  Arity = asm_genop:arity(Op),
  Args = [element(ArgIndex+1, Src) || ArgIndex <- lists:seq(1, Arity)],
  emit_gleam_op({Op, Args}, CState).

emit_gleam_op(Op, CState) when not is_list(Op) -> emit_gleam_op([Op], CState);
emit_gleam_op(Ops, CState = #compile_state{ir = Ir0}) ->
  %% TODO: ++ is O(N^2)
  CState#compile_state{ ir = Ir0 ++ Ops }.

compile_irop_dbg(Op, {Accum0, State0}) ->
  {Accum1, State1} = compile_irop(Op, {Accum0, State0}),
  Diff = case Accum1 == Accum0 of true -> []; false -> hd(Accum1) end,
  io:format("~p -> ~p~n", [Op, Diff]),
  {Accum1, State1}.

%% Catch label instruction and write its offset in bytes to separate map
%% compile_irop({label, [N]}, {Accum, Mod0}) ->
%%   Offset = iolist_size(Accum),
%%   %io:format("label ~p pos ~p~n", [N, Offset]),
%%   {Accum, asm_module:register_label(N, Offset, Mod0)};
compile_irop({Op, OpArgs}, {Accum, Mod0}) ->
  CompileArg = fun(Arg, {Accum_, M}) ->
                 {BinArg, M1} = asm_irop:encode_arg(Arg, M),
                 {[BinArg | Accum_], M1}
               end,
  {Args, Mod} = lists:foldr(CompileArg, {[], Mod0}, OpArgs),
  Compiled = [asm_genop:opcode(Op) | Args],
  {[Compiled | Accum], Mod}.

