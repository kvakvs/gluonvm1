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
        , find_or_create_atom/2]).

-record(asm_module, { id
                    , options = orddict:from_list([{line_numbers, true}])
                    , features = ordsets:new()
                    , code = []
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
