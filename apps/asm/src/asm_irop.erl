%%%-------------------------------------------------------------------
%%% @doc Opcodes for gluon
%%% @end
%%%-------------------------------------------------------------------
-module(asm_irop).

%% API
-export([integer/1
        , uint_enc/1
%%         , comment/1, 'LINE'/3, 'MOVE'/3, 'PUSH'/2, 'POP'/2, 'CALL'/3
%%        , 'CALL'/5, 'TAILCALL'/3, 'RET'/0, 'TEST'/4, 'GET_LIST'/4, 'LABEL'/1
        , encode_arg/2, get_test_type/1]).

-include("../../emuemu/src/emu.hrl").

%% -define(label,      0).
%% -define(line_info,  1).
%% -define(push,       2).
%% -define(pop,        3).
%% -define(call,       4).
%% -define(tailcall,   5).
%% -define(return,     6).
%% -define(test,       7).

%% Opcode groups, 2 bits (0..3)
%% -define(op_group_control, 0).
%% -define(op_group_move,    1).
%% %-define(op_group_?,   2).

%% Byte tags to mark value sign or origin of value, tag for integer is optional
%% when sign is known.
-define(tag_integer_pos, 255).
-define(tag_integer_neg, 254).
-define(tag_atom, 253).
-define(tag_label, 252).
-define(tag_mfarity, 251).
-define(tag_register, 250).
-define(tag_stack, 249).
-define(tag_nil, 248).
-define(tag_literal, 247).
-define(tag_fp_register, 246).

%% Argument type, small to fit in 3bit field (0..7)
%% -define(arg_register,  0).
%% -define(arg_stack,     1).
%% -define(arg_literal,   2).
%% -define(arg_immediate, 3).
%% %% argument means amount of N live registers (for function calls)
%% -define(arg_live_registers, 4).

get_test_type(is_lt) ->         0;
get_test_type(is_ge) ->         1;
get_test_type(is_eq) ->         2;
get_test_type(is_ne) ->         3;
get_test_type(is_eq_exact) ->   4;
get_test_type(is_ne_exact) ->   5;
get_test_type(is_integer) ->    6;
get_test_type(is_float) ->      7;
get_test_type(is_number) ->     8;
get_test_type(is_atom) ->       9;
get_test_type(is_pid) ->        10;
get_test_type(is_reference) ->  11;
get_test_type(is_port) ->       12;
get_test_type(is_nil) ->        13;
get_test_type(is_binary) ->     14;
get_test_type(is_list) ->       15;
get_test_type(is_nonempty_list)->16;
get_test_type(is_tuple) ->      17;
get_test_type(is_boolean) ->    18;
get_test_type(is_function) ->   19.

%% @doc Tagged varlength integer, tag specifies the sign
integer(N) when N < 0 -> <<?tag_integer_neg, (uint_enc(-N))/binary>>;
integer(N) -> <<?tag_integer_pos, (uint_enc(N))/binary>>.

%% @doc Varlength encoding with highest bit set to 1 for continuation and to 0
%% for last segment of 7-bit sequence. Only positive integers.
uint_enc(N) when N >= 0 ->
  Tail = integer_enc(N band 127, <<>>, 0),
  case N < 128 of
    true -> Tail;
    false ->
      Head = integer_enc(N div 128, <<>>, 1),
      <<Head/binary, Tail/binary>>
  end.

integer_enc(N, Acc, FlagBit) when is_integer(N), N > 127 ->
  Part = N band 127,
  integer_enc(N div 128, <<FlagBit:1, Part:7, Acc/binary>>, FlagBit);
integer_enc(N, Acc, FlagBit) when is_integer(N) ->
  <<FlagBit:1, N:7, Acc/binary>>.

%%%
%%% Opcode structure encoding and group tag
%%%
%%% [7  6  5  4  3  2  1  0  ] opcode bits
%%%  0..0  Op------ Type----   control opcode defining jumps, labels, stack etc
%%%  0..1  Src----- Dst-----   move opcode with src/dst types encoded, and data
%%%                            will follow as varlength integer
%%%  1..0  Src-----            Push and pop operations
%% @ doc Opcode is a byte with 2 bits tagging the operation type
%% opcode(_Group=?op_group_control, Op, DataType) ->
%%   <<?op_group_control:2, Op:3, DataType:3>>.

%% @ doc Move opcode contains tagged types for source and destination
%% opcode(_Group=?op_group_data, Src, Dst) -> <<?op_group_data:2, Src:3, Dst:3>>.

%% @ doc Encode {f,Label} as var-int Label, atom is encoded as reference to atom
%% table in Module#asm_module{}
%% label_enc(A, MState) when is_atom(A) ->
%%   atom_ref_enc(A, MState);
%% label_enc({extfunc, M, F, Arity}, MState) ->
%%   {MEnc, MState1} = atom_ref_enc(M, MState),
%%   {FEnc, MState2} = atom_ref_enc(F, MState1),
%%   {{extfunc, MEnc, FEnc, Arity}, MState2};
%% label_enc({f, Lbl}, MState) ->
%%   {Lbl, MState}.

atom_ref_enc(A, Mod0) -> asm_module:find_or_create_atom(A, Mod0).
literal_ref_enc(L, Mod0) -> asm_module:find_or_create_literal(L, Mod0).

%% @ doc Encodes a tagged value to intermediate format, converts atoms to
%% references to atom table for example
%% value_enc(L, MState) when is_list(L) ->
%%   lists:foldl(fun(Val, {Acc, MSt}) ->
%%                 {Val1, MSt1} = value_enc(Val, MSt),
%%                 {[Val1 | Acc], MSt1}
%%               end, {[], MState}, L);
%% value_enc(?nil, MState) ->
%%   {?nil, MState};
%% value_enc({live_registers, Num}, MState) -> % number of live registers to save/restore
%%   {{'$LIVE', Num}, MState};
%% value_enc({x, Reg}, MState) -> % value is register cell
%%   {{'$REG', Reg}, MState};
%% value_enc({y, Stack}, MState) -> % value is stack cell
%%   {{'$STACK', Stack}, MState};
%% value_enc({atom, Atom}, MState) -> % value is reference to atom table
%%   atom_ref_enc(Atom, MState);
%% value_enc({integer, Imm}, MState) -> % value is immediately available in code
%%   {{'$IMM', Imm}, MState};
%% value_enc({literal, Value}, MState) -> % value is immediately available in code
%%   literal_ref_enc(Value, MState).

encode_atom(A, Mod0) ->
  {Index, Mod} = atom_ref_enc(A, Mod0),
  {<<?tag_atom:8, (uint_enc(Index))/binary>>, Mod}.

encode_arg({x, X}, Mod) when is_integer(X), X >= 0 ->
  %% TODO: Shorten this for low values of X to 1 byte
  {<<?tag_register:8, (uint_enc(X))/binary>>, Mod};
encode_arg({y, Y}, Mod) when is_integer(Y), Y >= 0 ->
  {<<?tag_stack:8, (uint_enc(Y))/binary>>, Mod};
encode_arg({atom, Atom}, Mod0) when is_atom(Atom) ->
  encode_atom(Atom, Mod0);
encode_arg(Atom, Mod0) when is_atom(Atom) ->
  encode_atom(Atom, Mod0);
encode_arg({integer, N}, Mod) ->
  {integer(N), Mod};
encode_arg(N, Mod) when is_integer(N) ->
  {integer(N), Mod};
encode_arg(nil, Mod) ->
  {<<?tag_nil:8>>, Mod};
encode_arg({f, L}, Mod) ->
  {<<?tag_label:8, (uint_enc(L))/binary>>, Mod};
%% encode_arg({'char', C}, Dict) ->
%%     {encode(?tag_h, C), Dict};
encode_arg({string, String}, Mod0) ->
  encode_arg({literal, String}, Mod0);
encode_arg({extfunc, M, F, A}, Mod0) ->
  encode_arg({literal, {M,F,A}}, Mod0);
%% encode_arg({list, List}, Dict0) ->
%%   {L, Dict} = encode_list(List, Dict0, []),
%%   {[encode(?tag_z, 1), encode(?tag_u, length(List))|L], Dict};
encode_arg({float, Float}, Dict) when is_float(Float) ->
  encode_arg({literal,Float}, Dict);
encode_arg({fr,Fr}, Mod) ->
  {<<?tag_fp_register:8, (uint_enc(Fr))/binary>>, Mod};
encode_arg({field_flags,Flags0}, Mod) ->
  Flags = lists:foldl(fun (F, S) -> S bor flag_to_bit(F) end, 0, Flags0),
  encode_arg({literal, Flags}, Mod);
%% encode_arg({alloc,List}, Dict) ->
%%   encode_alloc_list(List, Dict);
encode_arg({literal, Term}, Mod0) ->
  {Offset, Mod} = literal_ref_enc(Term, Mod0),
  {<<?tag_literal:8, (uint_enc(Offset))/binary>>, Mod};
encode_arg([{location, File, Line}], Mod0) ->
  {LitId, Mod1} = literal_ref_enc(File, Mod0),
  {<<?tag_label:8, (uint_enc(LitId))/binary, (uint_enc(Line))/binary>>, Mod1};
encode_arg([], Mod0) ->
  encode_arg(nil, Mod0).

%%flag_to_bit(aligned) -> 16#01; %% No longer useful.
flag_to_bit(little)  -> 16#02;
flag_to_bit(big)     -> 16#00;
flag_to_bit(signed)  -> 16#04;
flag_to_bit(unsigned)-> 16#00;
%%flag_to_bit(exact)   -> 16#08;
flag_to_bit(native)  -> 16#10;
flag_to_bit({anno,_}) -> 0.

%% arg_list_encode([], Accum) -> Accum;
%% arg_list_encode([Arg | Tail], Accum) ->
%%   {ArgType, ArgEncoded} = arg_encode(Arg),
%%   arg_list_encode(Tail, <<ArgType:8/binary, ArgEncoded/binary, Accum/binary>>).

%%%
%%% Specific opcodes
%%%

%% 'LABEL'(N) -> #{irop => {'LABEL', N}}.
%% comment(Args) -> list_to_tuple(['%' | Args]).
%%
%% 'LINE'(Filename, Line, MState) ->
%%   {FilenameEnc, MState1} = literal_ref_enc(Filename, MState),
%%   {#{irop => {'LINE', FilenameEnc, Line}}
%%   , MState1}.
%% 'MOVE'(Src, Src, MState) -> {nop, MState};
%% 'MOVE'(Src, Dst, MState) ->
%%   {SrcEnc, MState1} = value_enc(Src, MState),
%%   {DstEnc, MState2} = value_enc(Dst, MState1),
%%   {#{irop => {'MOVE', SrcEnc, DstEnc}}
%%   , MState2}.
%% 'PUSH'(Src, MState) ->
%%   {SrcEnc, MState1} = value_enc(Src, MState),
%%   {#{irop => {'PUSH', SrcEnc}}
%%   , MState1}.
%% 'POP'(Dst, MState) ->
%%   {DstEnc, MState1} = value_enc(Dst, MState),
%%   {#{irop => {'POP', DstEnc}}
%%   , MState1}.
%% %% TODO: Separate shorter call instructions when ResultDst is {x,0}
%% %% TODO: Maybe separate opcode for bif/nonbif call?
%% 'CALL'(Label, Arity, IsBif, ResultDst, MState) ->
%%   {LabelEnc, MState1} = label_enc(Label, MState),
%%   {ResultDstEnc, MState2} = value_enc(ResultDst, MState1),
%%   {#{irop => {'CALL', LabelEnc, Arity, IsBif, ResultDstEnc}}
%%   , MState2}.
%% 'CALL'(Label, Arity, MState) ->
%%   {LabelEnc, MState1} = label_enc(Label, MState),
%%   {#{irop => {'CALL', LabelEnc, Arity}}
%%   , MState1}.
%% 'TAILCALL'(Arity, Label, MState) ->
%%   {LabelEnc, MState1} = label_enc(Label, MState),
%%   {#{irop => {'TAILCALL', Arity, LabelEnc}}
%%   , MState1}.
%% 'RET'() -> #{irop => 'RET'}.
%% 'TEST'(Test, Label, Args, MState) ->
%%   {LabelEnc, MState1} = label_enc(Label, MState),
%%   {ArgsEnc, MState2} = lists:foldr(fun fold_arg_enc/2, {[], MState1}, Args),
%%   {#{irop => {'TEST', Test, LabelEnc, ArgsEnc}}
%%    , MState2}.
%% 'GET_LIST'(Src, OutHead, OutTail, MState) ->
%%   {SrcEnc,     MState1} = value_enc(Src, MState),
%%   {OutHeadEnc, MState2} = value_enc(OutHead, MState1),
%%   {OutTailEnc, MState3} = value_enc(OutTail, MState2),
%%   {#{irop => {'GET_LIST', SrcEnc, OutHeadEnc, OutTailEnc}}
%%   , MState3}.
%%
%% fold_arg_enc(Arg, {Acc, MState}) ->
%%   {Encoded, MState1} = value_enc(Arg, MState),
%%   {[Encoded | Acc], MState1}.

%% ref(X) -> {'$REF', X}.

%% compile({'LABEL', N}) ->
%%   <<(opcode(?op_group_control, ?label, 0))/binary, (uint_enc(N))/binary>>;
%% compile({'LINE', _Filename, Line}) ->
%%   <<(opcode(?op_group_control, ?line_info, 0))/binary, (uint_enc(Line))/binary>>;
%% %% compile({'MOVE', Src, Dst}) ->
%% %%   {SrcT, SrcEncoded} = get_arg_type_and_encoded(Src),
%% %%   {DstT, DstEncoded} = get_arg_type_and_encoded(Dst),
%% %%   <<?op_group_move:2, SrcT:3, DstT:3, SrcEncoded/binary, DstEncoded/binary>>;
%% %% compile({'PUSH', Src}) ->
%% %%   {SrcT, SrcEnc} = get_arg_type_and_encoded(Src),
%% %%   <<?op_group_control:2, ?push:3, SrcT:3, SrcEnc/binary>>;
%% %% compile({'POP', Dst}) ->
%% %%   {DstT, DstEnc} = get_arg_type_and_encoded(Dst),
%% %%   <<?op_group_control:2, ?pop:3, DstT:3, DstEnc/binary>>;
%% compile({'CALL', LabelEnc, IsBif}) ->
%%   <<?op_group_control:2, ?call:3, IsBif:3, (compile(LabelEnc))/binary>>;
%% compile({'TAILCALL', Arity, LabelEnc}) ->
%%   <<?op_group_control:2, ?tailcall:3, 0:3
%%     , (uint_enc(Arity))/binary, (compile(LabelEnc))/binary>>;
%% compile({'RET'}) ->
%%   <<?op_group_control:2, ?return:3, 0:3>>;
%% compile({'TEST', Test, LabelEnc, ArgsEnc}) ->
%%   <<?op_group_control:2, ?test:3, 0:3
%%     , (get_test_type(Test)):8
%%     , (compile(LabelEnc))/binary
%%     , (compile(ArgsEnc))/binary>>.
