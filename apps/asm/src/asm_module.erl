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
        , set_ir/2
        , get_ir/1
        , set_bin/2
        , find_atom/2
        , find_or_create_atom/2
        , find_literal/2
        , find_or_create_literal/2
        , add_fun/4
        , to_binary/1
        , register_label/3
        , get_name/1
        , funarity_to_label/2
        , to_proplist/1
        ]).

%% funs contains {F,Arity} -> Label mapping where F is reference to atoms table
-record(asm_module, {name
                    , options = orddict:from_list([{line_numbers, true}])
                    , features = ordsets:new()
                    , ir = []     % ir made of homogenized erlang asm(.S) tuples
                    , bin = <<>>  % binary gleam/beam code
                    , funs = orddict:new()
                    , exports = orddict:new()
                    , atom_counter = -1
                    , atoms = orddict:new()
                    , literal_counter = -1
                    , literals = orddict:new()
                    , labels = orddict:new()
                    }).

new(Id) -> #asm_module{name=Id}.
to_proplist(#asm_module{} = M) ->
  lists:zip(record_info(fields, asm_module), tl(tuple_to_list(M))).

get_option(Key, #asm_module{options=Opt}) ->
  case orddict:find(Key, Opt) of
    {ok, Value} -> Value;
    error -> undefined
  end.

has_feature(Key, #asm_module{features=Ft}) ->
  orddict:is_key(Key, Ft).

%% @doc Registers a feature as used (can't be unregistered). Features are
%% calculated from code being compiled then are compared with wanted features.
%% If user wants to have less features, than code requires, error is produced.
set_feature(Key, M=#asm_module{features = Ft}) ->
  M#asm_module{features = orddict:store(Key, true, Ft)}.

%% Takes raw {atom Fun, int Arity} from asm source and converts into
%% {atom index
set_exports(Exports0, M0) ->
  {Exports, M} = lists:foldl(fun map_export/2, {[], M0}, Exports0),
  M#asm_module{exports = Exports}.

%% @private
map_export({F, Arity}, {Accum, Mod}) ->
  {FIndex, Mod1} = find_or_create_atom(F, Mod),
  {[{FIndex, Arity} | Accum], Mod1}.

set_ir(IR, M) -> M#asm_module{ir = IR}.
get_ir(#asm_module{ir=IR}) -> IR.

set_bin(Bin, M) -> M#asm_module{bin=Bin}.

get_name(#asm_module{name=Name}) -> Name.

find_or_create_atom(A, M=#asm_module{atoms=Atoms, atom_counter=Counter}) ->
  {Index, Counter1, Atoms1}
    = case orddict:find(A, Atoms) of
        {ok, I} -> {I, Counter, Atoms};
        error ->
          I = Counter+1,
          {I, I, orddict:store(A, I, Atoms)}
      end,
  {Index, M#asm_module{atom_counter=Counter1, atoms=Atoms1}}.

find_atom(AtomIndex, #asm_module{atoms=Atoms}) ->
  find_atom(AtomIndex, Atoms);
find_atom(AtomIndex, Atoms) ->
  case lists:keysearch(AtomIndex, 2, Atoms) of
    {value, {Atom, _}} -> {ok, Atom};
    false -> {error, not_found}
  end.

find_or_create_literal(Lit, M=#asm_module{ literals=Literals
                                         , literal_counter=Counter}) ->
  {Index, Counter1, Literals1}
    = case orddict:find(Lit, Literals) of
        {ok, I} -> {I, Counter, Literals};
        error ->
          I = Counter+1,
          {I, I, orddict:store(Lit, I, Literals)}
      end,
  {Index, M#asm_module{literal_counter=Counter1, literals=Literals1}}.

find_literal(LitIndex, #asm_module{literals=Literals}) ->
  {value, {Lit, _}} = lists:keysearch(LitIndex, 2, Literals),
  {ok, Lit}.

add_fun(Fun, Arity, Label, MState=#asm_module{funs=Funs}) when is_atom(Fun) ->
  {FunAtomIndex, M1} = asm_module:find_or_create_atom(Fun, MState),
  Funs1 = orddict:store({FunAtomIndex, Arity}, Label, Funs),
  M1#asm_module{funs = Funs1}.

to_binary({labels, LDict}) ->
  %(asm_irop:uint_enc(Label))/binary,
  Out = << <<(asm_irop:uint_enc(Pos))/binary>>
        || {_Label, Pos} <- orddict:to_list(LDict) >>,
  LenLabels = asm_irop:uint_enc(length(LDict)),
  <<"LABL"
  , (asm_irop:uint_enc(byte_size(Out) + byte_size(LenLabels)))/binary
  , LenLabels/binary
  , Out/binary>>;
to_binary({code, Code}) when is_binary(Code) ->
  CodeSize = byte_size(Code),
  io:format("code size ~p~n", [CodeSize]),
  <<"CODE", (asm_irop:uint_enc(CodeSize))/binary, Code/binary>>;
to_binary({funs, Funs}) ->
  Out0 = << <<(asm_irop:uint_enc(FunAtom))/binary
            , (asm_irop:uint_enc(Arity))/binary
            , (asm_irop:uint_enc(LabelId))/binary>>
    || {{FunAtom, Arity}, LabelId} <- Funs>>,
  Out = iolist_to_binary(Out0),
  LenFuns = asm_irop:uint_enc(length(Funs)),
  <<"FUNT"
  , (asm_irop:uint_enc(byte_size(Out) + byte_size(LenFuns)))/binary
  , LenFuns/binary
  , Out/binary>>;
to_binary({exports, Exports}) ->
  Out0 = << <<(asm_irop:uint_enc(FunAtom))/binary
           , (asm_irop:uint_enc(Arity))/binary>>
        || {FunAtom, Arity} <- Exports>>,
  Out = iolist_to_binary(Out0),
  LenExports = asm_irop:uint_enc(length(Exports)),
  <<"EXPT"
  , (asm_irop:uint_enc(byte_size(Out) + byte_size(LenExports)))/binary
  , LenExports/binary
  , Out/binary>>;
to_binary({atoms, AtomsDict}) ->
  %% Atom table is encoded as "Atom" + var_int BytesLength + var_int AtomsCount,
  %% then each atom is encoded as var_int Bytes + utf8 Atom
  AtomEnc = fun(Atm) ->
    ABin = atom_to_binary(Atm, utf8),
    <<(asm_irop:uint_enc(byte_size(ABin)))/binary
    , ABin/binary>>
  end,
  SortFun = fun({KeyA,ValA}, {KeyB,ValB}) -> {ValA,KeyA} =< {ValB,KeyB} end,
  Atoms = lists:sort(SortFun, orddict:to_list(AtomsDict)),
  Out = iolist_to_binary([AtomEnc(Atom) || {Atom, _} <- Atoms]),
  LenAtoms = asm_irop:uint_enc(length(Atoms)),
  <<"ATOM"
  , (asm_irop:uint_enc(byte_size(Out) + byte_size(LenAtoms)))/binary
  , LenAtoms/binary
  , Out/binary>>;
to_binary({literals, LitDict}) ->
  %% Literal table is encoded as "LitT" + var_int BytesLength + var_int Count,
  %% then each literal is encoded as external term format
  %% ASSUMPTION: orddict:to_list gives sorted ascending order without skips
  Literals = orddict:to_list(LitDict),
  Enc = fun(L) ->
          LBin = term_to_binary(L),
          <<(asm_irop:uint_enc(byte_size(LBin)))/binary, LBin/binary>>
        end,
  Out = iolist_to_binary([Enc(L) || {L, _} <- Literals]),
  LenLiterals = asm_irop:uint_enc(length(Literals)),
  <<"LTRL"
    , (asm_irop:uint_enc(byte_size(Out) + byte_size(LenLiterals)))/binary
    , LenLiterals/binary
    , Out/binary>>;
to_binary(#asm_module{name=Name, bin=Bin, literals=Lit, labels=Labels
                     , exports=Exports, atoms=Atoms, funs=Funs}) ->
  ModName = atom_to_binary(Name, utf8),
  %% Module file is encoded as "GLEAM" + module name length + module name
  %%   + chunks (4 byte name, var_int byte_size)
  <<"GLEAM"
    , (byte_size(ModName)):8, ModName/binary
    , (to_binary({atoms, Atoms}))/binary
    , (to_binary({funs, Funs}))/binary
    , (to_binary({exports, Exports}))/binary
    , (to_binary({code, Bin}))/binary
    , (to_binary({literals, Lit}))/binary
    , (to_binary({labels, Labels}))/binary
  >>.

register_label(Label, Position, #asm_module{labels=Labels}=M) ->
  error = orddict:find(Label, Labels),
  Labels1 = orddict:store(Label, Position, Labels),
  M#asm_module{labels=Labels1}.

funarity_to_label(#asm_module{funs=Funs}, {F, Arity}) ->
  case lists:keyfind({F, Arity}, 1, Funs) of
    false -> {error, not_found};
    {{F, Arity}, Label} -> {ok, Label}
  end.
