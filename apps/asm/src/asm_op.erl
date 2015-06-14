%%%-------------------------------------------------------------------
%%% @doc Opcodes for gluon
%%% @end
%%%-------------------------------------------------------------------
-module(asm_op).

%% API
-export(['LABEL'/1
        , integer/1
        , uint_enc/1
        , 'LINE'/2
        , 'MOVE'/2
        , 'PUSH'/1
        , 'POP'/1
        , 'CALL'/3
        , 'TAILCALL'/3
        ]).

-define(label,      0).
-define(line_info,  1).
-define(push,       2).
-define(pop,        3).
-define(call,       4).
-define(tailcall,   5).

%% Byte tags to mark value sign or origin of value, tag for integer is optional
%% when sign is known.
-define(tag_integer_pos, 255).
-define(tag_integer_neg, 254).
-define(tag_atom, 253).
-define(tag_label, 252).

%% @doc Tagged varlength integer, tag specifies the sign
integer(N) when N < 0 -> <<?tag_integer_neg, (uint_enc(-N))/binary>>;
integer(N) -> <<?tag_integer_pos, (uint_enc(N))/binary>>.

%% @doc Varlength encoding with highest bit set to 1 for continuation and to 0
%% for last segment of 7-bit sequence. Only positive integers.
uint_enc(N) -> integer_enc(N, <<>>).

integer_enc(N, Acc) when N > 127 ->
  Part = N band 127,
  integer_enc(N div 128, <<1:1, Part:7, Acc/binary>>);
integer_enc(N, Acc) ->
  <<0:1, N:7, Acc/binary>>.

%%%
%%% Opcode structure encoding and group tag
%%%
%%% [7  6  5  4  3  2  1  0  ] opcode bits
%%%  0..0  Op------ Type----   control opcode defining jumps, labels, stack etc
%%%  0..1  Src----- Dst-----   move opcode with src/dst types encoded, and data
%%%                            will follow as varlength integer
%%%  1..0  Src-----            Push and pop operations

%% Opcode groups, 2 bits (0..3)
-define(op_group_control, 0).
-define(op_group_move,    1).
%-define(op_group_stack,   2).

%% @doc Opcode is a byte with 2 bits tagging the operation type
opcode(_Group=?op_group_control, Op, DataType) ->
  <<?op_group_control:2, Op:3, DataType:3>>.

%% @ doc Move opcode contains tagged types for source and destination
%% opcode(_Group=?op_group_data, Src, Dst) -> <<?op_group_data:2, Src:3, Dst:3>>.

%% Argument type, small to fit in 3bit field (0..7)
-define(arg_register,  0).
-define(arg_stack,     1).
-define(arg_literal,   2).
-define(arg_immediate, 3).

%% @doc Encode {f,Label} as var-int Label, atom is encoded as reference to atom
%% table in Module#asm_module{}
label_enc(A, MState) when is_atom(A) -> atom_ref_enc(A, MState);
label_enc({f, Lbl}, MState) ->
  {<<?tag_label:8, (uint_enc(Lbl))/binary>>, MState}.

%% @private
atom_ref_enc(A, MState) ->
  {Index, Module1} = asm_module:find_or_create_atom(A, MState),
  {<<?tag_atom:8, (uint_enc(Index))/binary>>, Module1}.

%% @doc Returns tuple {arg 3-bit tag for opcode, and encoded arg as integer}
arg_encode({x, Reg}) ->
  {?arg_register, uint_enc(Reg)};
arg_encode({y, Stack}) ->
  {?arg_stack, uint_enc(Stack)};
arg_encode({atom, _LiteralId}) -> % value is taken from table
  {?arg_literal, uint_enc(0)};
arg_encode({integer, Imm}) -> % value is immediately encoded in code
  {?arg_immediate, uint_enc(Imm)}.

%% arg_list_encode([], Accum) -> Accum;
%% arg_list_encode([Arg | Tail], Accum) ->
%%   {ArgType, ArgEncoded} = arg_encode(Arg),
%%   arg_list_encode(Tail, <<ArgType:8/binary, ArgEncoded/binary, Accum/binary>>).

%%%
%%% Specific opcodes
%%%

%% @doc Label is tagged with 1, followed by headerless varlength integer
'LABEL'(N) ->
  {'LABEL', <<(opcode(?op_group_control, ?label, 0))/binary
            , (uint_enc(N))/binary>>}.

%% @doc Line numbers information, marks line omitting the file (TODO: filename)
'LINE'(_Filename, Line) ->
  {'LINE', <<(opcode(?op_group_control, ?line_info, 0))/binary
           , (uint_enc(Line))/binary>>}.

%% @doc Label is tagged with 1, followed by headerless varlength integer
'MOVE'(Src, Dst) ->
  {SrcT, SrcEncoded} = arg_encode(Src),
  {DstT, DstEncoded} = arg_encode(Dst),
  {'MOVE', <<?op_group_move:2, SrcT:3, DstT:3
           , SrcEncoded/binary, DstEncoded/binary>>}.

'PUSH'(Src) ->
  {SrcT, SrcEnc} = arg_encode(Src),
  {'PUSH', <<?op_group_control:2, ?push:3, SrcT:3, SrcEnc/binary>>}.

'POP'(Dst) ->
  {DstT, DstEnc} = arg_encode(Dst),
  {'POP', <<?op_group_control:2, ?pop:3, DstT:3, DstEnc/binary>>}.

'CALL'(Label, IsBif, MState) ->
  {LabelEnc, Module1} = label_enc(Label, MState),
  {{'CALL', <<?op_group_control:2, ?call:3, IsBif:3
           , (LabelEnc)/binary>>
   }, Module1}.

'TAILCALL'(Arity, Label, MState) ->
  {LabelEnc, Module1} = label_enc(Label, MState),
  {{'TAILCALL', <<?op_group_control:2, ?tailcall:3, 0:3
                , (uint_enc(Arity))/binary
                , (LabelEnc)/binary>>
   }, Module1}.

%% call_bif(Lbl, Live, Bif, Args, DstReg) ->
%%   {_RegT, DstRegEncoded} = arg_encode(DstReg),
%%   %% TODO: split call into push/prepare args + call + move result
%%   <<(opcode(?op_group_control, ?call_bif))/binary
%%     , (label_encode(Lbl))/binary
%%     , (integer_enc(Live, <<>>))/binary
%%     , (integer_enc(Bif, <<>>))/binary
%%     , (arg_list_encode(Args, <<>>))/binary
%%     , DstRegEncoded/binary>>.
