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
%%         , register_label/3
        , write_ir/2
        , read_ir/1
        , get_name/1
        , funarity_to_label/2
%%         , label_to_offset/2
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
                    %, labels = orddict:new()
                    }).

new(Id) -> #asm_module{name=Id}.

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

set_exports(Exports, M) -> M#asm_module{exports = Exports}.

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

add_fun(FunAtomIndex, Arity, Label, MState=#asm_module{funs=Funs}) ->
  Funs1 = orddict:store({FunAtomIndex, Arity}, Label, Funs),
  MState#asm_module{funs = Funs1}.

to_binary({code, Code}) when is_binary(Code) ->
  CodeSize = byte_size(Code),
  io:format("code size ~p~n", [CodeSize]),
  <<"CODE", (asm_irop:uint_enc(CodeSize))/binary, Code/binary>>;
to_binary({funs, _FunDict}) -> <<"LAMD", 0>>;
to_binary({exports, _ExportDict}) -> <<"EXPT", 0>>;
to_binary({atoms, AtomsDict}) ->
  %% Atom table is encoded as "Atom" + var_int BytesLength + var_int AtomsCount,
  %% then each atom is encoded as var_int Bytes + utf8 Atom
  AtomEnc = fun(Atm) ->
    ABin = atom_to_binary(Atm, utf8),
    <<(asm_irop:uint_enc(byte_size(ABin)))/binary
    , ABin/binary>>
  end,
  %% ASSUMPTION: orddict:to_list gives sorted ascending order without skips
  Atoms = orddict:to_list(AtomsDict),
  Out = iolist_to_binary([AtomEnc(Atom) || {Atom, _} <- Atoms]),
  <<"ATOM"
  , (asm_irop:uint_enc(byte_size(Out)))/binary
  , (asm_irop:uint_enc(length(Atoms)))/binary
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
  <<"LTRL"
    , (asm_irop:uint_enc(byte_size(Out)))/binary
    , (asm_irop:uint_enc(length(Literals)))/binary
    , Out/binary>>;
to_binary(#asm_module{name=Name, bin=Bin, literals=Lit
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
    , (to_binary({literals, Lit}))/binary>>.

%% write_ir(Filename, #asm_module{name=Name, ir=IR, atoms=At, literals=Lit
%%                               , funs=Funs}) ->
%%   Out = [ {name, Name}
%%         , {ir, IR}
%%         , {literals, Lit}
%%         , {atoms, At}
%%         , {funs, Funs}
%%         ],
%%   file:write_file(Filename, io_lib:format("~p.~n", [Out])).
%%
%% read_ir(Str) ->
%%   {ok, [IRFile]} = file:consult(Str),
%%   Name  = proplists:get_value(name, IRFile),
%%   IR    = proplists:get_value(ir, IRFile, []),
%%   Lit   = proplists:get_value(literals, IRFile, []),
%%   Atoms = proplists:get_value(atoms, IRFile, []),
%%   Funs  = read_ir_resolve_funs(proplists:get_value(funs, IRFile, []), Atoms),
%%   IR1 = read_ir_resolve_atoms(IR, Atoms, []),
%%   #asm_module{ name=Name
%%              , ir=IR1
%%              , atoms=Atoms
%%              , literals=Lit
%%              , funs=Funs}.

%% %% @private
%% resolve_irop_piece({'$ATOM', A}, Atoms) ->
%%   {ok, Atom} = find_atom(A, Atoms),
%%   Atom;
%% resolve_irop_piece({'$MFA', M0, F0, A}, Atoms) ->
%%   [{M, F}] = read_ir_resolve_atoms([{M0, F0}], Atoms, []),
%%   {'$MFA', M, F, A};
%% resolve_irop_piece(X, _) -> X.
%%
%% %% @private
%% resolve_irop(#{irop := Irop} = Line, Atoms) ->
%%   maps:put(irop, resolve_irop_piece(Irop, Atoms), Line);
%% resolve_irop(#{irops := IropList} = Line, Atoms) ->
%%   ResolveEach = fun(X) -> resolve_irop(X, Atoms) end,
%%   maps:put(irops, lists:map(ResolveEach, IropList), Line).
%%
%% %% @private
%% read_ir_resolve_atoms([], _Atoms, Accum) -> lists:reverse(Accum);
%% read_ir_resolve_atoms([Line | Tail], Atoms, Accum) ->
%%   Line1 = resolve_irop(Line, Atoms),
%%   read_ir_resolve_atoms(Tail, Atoms, [Line1 | Accum]).

%% %% @private
%% read_ir_resolve_funs(Funs, Atoms) ->
%%   ResolveFun = fun({{FunAtom, Arity}, FunIndex}) ->
%%       case lists:keyfind(FunAtom, 2, Atoms) of
%%         false ->
%%           erlang:error({broken_ir_file
%%                        , 'cant resolve atom in fun name'
%%                        , {atom, FunAtom, arity, Arity}
%%                        });
%%         {Atom, _AtomIndex} -> {{Atom, Arity}, FunIndex}
%%       end
%%     end,
%%   lists:map(ResolveFun, Funs).

%% register_label(Label, Position, #asm_module{labels=Labels}=M) ->
%%   error = orddict:find(Label, Labels),
%%   Labels1 = orddict:store(Label, Position, Labels),
%%   M#asm_module{labels=Labels1}.

funarity_to_label(#asm_module{funs=Funs}, {F, Arity}) ->
  case lists:keyfind({F, Arity}, 1, Funs) of
    false -> {error, not_found};
    {{F, Arity}, Label} -> {ok, Label}
  end.

%% label_to_offset(_, {error,_}=E) -> E; % sprinkle with a pinch of monad
%% label_to_offset(M, {ok,L}) -> label_to_offset(M, L);
%% label_to_offset(#asm_module{labels=Labels}, Label) ->
%%   case lists:keyfind(Label, 1, Labels) of
%%     false -> {error, not_found};
%%     {Label, Offset} -> {ok, Offset}
%%   end.
