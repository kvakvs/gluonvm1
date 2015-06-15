%%%-------------------------------------------------------------------
%%% @doc Wrapper for module record and some logic on accessing/updating state
%%% @end
%%%-------------------------------------------------------------------
-module(asm_module).

%% API
-export([new/1
        , get_option/2
        , has_feature/2
        , set_feature/2
        , set_exports/2
        , set_code/2
        , find_or_create_atom/2
        , add_fun/4
        , to_binary/1
        ]).

%% funs contains {F,Arity} -> Label mapping where F is reference to atoms table
-record(asm_module, { id
                    , options = orddict:from_list([{line_numbers, true}])
                    , features = ordsets:new()
                    , code = []
                    , funs = orddict:new()
                    , exports = orddict:new()
                    , atom_counter = 0
                    , atoms = orddict:new()
                    }).

new(Id) -> #asm_module{id=Id}.

get_option(Key, #asm_module{options=Opt}) ->
  case orddict:find(Key, Opt) of
    {ok, Value} -> Value;
    error -> undefined
  end.

has_feature(Key, #asm_module{features = Ft}) ->
  orddict:is_key(Key, Ft).

%% @doc Registers a feature as used (can't be unregistered). Features are
%% calculated from code being compiled then are compared with wanted features.
%% If user wants to have less features, than code requires, error is produced.
set_feature(Key, M=#asm_module{features = Ft}) ->
  M#asm_module{features = orddict:store(Key, true, Ft)}.

set_exports(Exports, M) -> M#asm_module{exports = Exports}.
set_code(Code, M) -> M#asm_module{code = Code}.

find_or_create_atom(A, M=#asm_module{atoms=Atoms, atom_counter=Counter}) ->
  {Index, Counter1, Atoms1}
    = case orddict:find(A, Atoms) of
        {ok, I} -> {I, Counter, Atoms};
        error ->
          I = Counter+1,
          {I, I, orddict:store(A, I, Atoms)}
      end,
  {Index, M#asm_module{atom_counter=Counter1, atoms=Atoms1}}.

add_fun(FunAtomIndex, Arity, Label, MState=#asm_module{funs=Funs}) ->
  Funs1 = orddict:store({FunAtomIndex, Arity}, Label, Funs),
  MState#asm_module{funs = Funs1}.

to_binary({code, Code}) when is_binary(Code) ->
  <<"Code", (asm_op:uint_enc(byte_size(Code)))/binary, Code/binary>>;
to_binary({funs, _FunDict}) -> <<"FunT", 0>>;
to_binary({exports, _ExportDict}) -> <<"ExpT", 0>>;
to_binary({atoms, AtomsDict}) ->
  %% Atom table is encoded as "Atom" + var_int BytesLength + var_int AtomsCount,
  %% then each atom is encoded as var_int Bytes + utf8 Atom
  AtomEnc = fun(Atm) ->
      ABin = atom_to_binary(Atm, utf8),
      <<(asm_op:uint_enc(byte_size(ABin)))/binary
        , ABin/binary>>
    end,
  %% ASSUMPTION: orddict:to_list gives sorted ascending order without skips
  Atoms = orddict:to_list(AtomsDict),
  Out = iolist_to_binary([AtomEnc(Atom) || {Atom, _} <- Atoms]),
  <<"Atom"
    , (asm_op:uint_enc(byte_size(Out)))/binary
    , (asm_op:uint_enc(length(Atoms)))/binary
    , Out/binary>>;
to_binary(#asm_module{code=Code0, exports=Exports, atoms=Atoms, funs=Funs}) ->
  %% Strip [{'OP', Code}] and get [Code]
  Code = lists:map(fun({_Op, C}) -> C end, lists:flatten(Code0)),
  <<"GLEAM"
  , (to_binary({atoms, Atoms}))/binary
  , (to_binary({funs, Funs}))/binary
  , (to_binary({exports, Exports}))/binary
  , (to_binary({code, iolist_to_binary(Code)}))/binary>>.

