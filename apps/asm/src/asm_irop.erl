%%%-------------------------------------------------------------------
%%% @doc Opcodes for gluon
%%% @end
%%%-------------------------------------------------------------------
-module(asm_irop).

%% API
-export([tag_integer/1
        , uint_enc/1
        , encode_arg/2, get_test_type/1]).

-include("../../emuemu/src/emu.hrl").

%% Byte tags to mark value sign or origin of value, tag for integer is optional
%% when sign is known.
-define(tag_integer_pos, 255).
-define(tag_integer_neg, 254).
-define(tag_atom, 253).
-define(tag_label, 252).
%-define(tag_mfarity, 251).
-define(tag_register, 250).
-define(tag_stack, 249).
-define(tag_nil, 248).
-define(tag_literal, 247).
-define(tag_fp_register, 246).
-define(tag_list, 245).

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
tag_integer(N) when N < 0 -> <<?tag_integer_neg, (uint_enc(-N))/binary>>;
tag_integer(N) -> <<?tag_integer_pos, (uint_enc(N))/binary>>.

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

atom_ref_enc(A, Mod0) -> asm_module:find_or_create_atom(A, Mod0).
literal_ref_enc(L, Mod0) -> asm_module:find_or_create_literal(L, Mod0).

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
encode_arg(nil, Mod) ->
  {<<?tag_nil:8>>, Mod};
encode_arg(Atom, Mod0) when is_atom(Atom) ->
  encode_atom(Atom, Mod0);
encode_arg({integer, N}, Mod) ->
  {tag_integer(N), Mod};
encode_arg(N, Mod) when is_integer(N) ->
  {tag_integer(N), Mod};
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
encode_arg([{location, _File, Line}], Mod0) ->
  %%{LitId, Mod1} = literal_ref_enc(File, Mod0),
  %%{<<?tag_label:8, (uint_enc(LitId))/binary, (uint_enc(Line))/binary>>, Mod1};
  encode_arg({integer, Line}, Mod0);
encode_arg({list, L}, Mod0) ->
  EncElement = fun(Elem, {Accum, M0}) ->
      {Encoded, M1} = encode_arg(Elem, M0),
      {<<Accum/binary, Encoded/binary>>, M1}
    end,
  {Enc, Mod1} = lists:foldr(EncElement, {<<>>, Mod0}, L),
  {<<?tag_list:8, (uint_enc(length(L)))/binary, Enc/binary>>, Mod1};
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
