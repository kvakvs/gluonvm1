%%% File    : beam_load.erl
%%% Author  : Tony Rogvall <tony@iMac.local>
%%% Description : Beam loader
%%% Created :  9 Jan 2006 by Tony Rogvall <tony@iMac.local>

-module(prototype_load).

-export([wrap/2, file/1]).
-compile(export_all).

-import(lists, [reverse/1, foldl/3, foldr/3, map/2]).

-include_lib("compiler/src/beam_disasm.hrl").

%%
%% wrap all calls from a module M by creating a stubb module
%% with that name, and temorarily renaming the module M to '__M'
%%
wrap(File, Mod) ->
    case beam_disasm:file(File) of
        B = #beam_file{} ->
            Module  = B#beam_file.module,
            Exports = B#beam_file.labeled_exports,
            %% generate stub calls to Mod
            {L,FCode} =
                foldl(fun ({module_info,0,_},{L,Code}) ->
                              {L,Code};
                          ({module_info,1,_},{L,Code}) ->
                              {L,Code};
                          ({Func,Arity,_Label},{L,Code}) ->
                              {L+2,
                               [{function,Func,Arity,L+1,
                                 [{label,L},
                                  {func_info,{atom,Module},{atom,Func},Arity},
                                  {label,L+1},
                                  {call_ext_only,Arity,
                                   {extfunc,Mod,Func,Arity}}]} | Code]}
                      end, {1,[]}, Exports),
            Asm =
                FCode ++
                [{function, module_info, 0, L+1,
                  [
                   {label,L},
                   {func_info,{atom,Module},{atom,module_info},0},
                   {label,L+1},
                   {move,{atom,Mod},{x,0}},
                   {call_ext_only,1,{extfunc,erlang,get_module_info,1}}]},

                 {function, module_info, 1, L+3,
                  [
                   {label,L+2},
                   {func_info,{atom,Module},{atom,module_info},1},
                   {label,L+3},
                   {move,{x,0},{x,1}},
                   {move,{atom,Mod},{x,0}},
                   {call_ext_only,2,{extfunc,erlang,get_module_info,2}}]}
                 ],
            Exp = map(fun({function,Name,Arity,_Entry,_Code}) ->
                              {Name,Arity}
                      end, Asm),
            Code = {Module,Exp,[],Asm,L+4},
            beam_asm:module(Code, [], File, []);
        Err ->
            Err
    end.

file(Mod) when is_atom(Mod) ->
    case code:which(Mod) of
        File when is_list(File) ->
            file(File);
        Reason when is_atom(Reason) ->
            {error,Reason}
    end;
file(File) when is_list(File) ->
    case beam_disasm:file(File) of
        B = #beam_file{} ->
            {ok,{LCode,Store}} = load_code(B#beam_file.code,1,[],dict:new()),
            Exp = map(fun({F,A,L}) ->
                              case dict:find(L, Store) of
                                  error -> {{F,A},undefined};
                                  {ok,Pos} ->  {{F,A},Pos}
                              end
                      end, B#beam_file.labeled_exports),
            {ok,{B#beam_file.module,Exp,LCode}};
        Err ->
            Err
    end.


load_code([{function,Name,Arity,Entry,FCode}|Code], Pos, LCode, Store) ->
    load_fcode(FCode, {{Name,Arity},Entry}, Code, Pos, LCode, Store);
load_code([], _Pos, LCode, Store) ->
    resolve_code(LCode, [], Store).

load_fcode([{label,L}|FCode],FAE,Code,Pos,LCode,Store) ->
    Store1 = dict:store(L, Pos, Store),
    load_fcode(FCode,FAE,Code,Pos,LCode,Store1);
load_fcode([{line,L}|FCode],FAE,Code,Pos,LCode,Store) ->
    Store1 = dict:store({line,L}, Pos, Store),
    load_fcode(FCode,FAE,Code,Pos,LCode,Store1);
load_fcode([Instr|FCode],FAE,Code,Pos,LCode,Store) ->
    load_fcode(FCode,FAE,Code,Pos+1,[Instr|LCode],Store);
load_fcode([],{FA,E},Code,Pos,LCode,Store) ->
    {ok,PosE} = dict:find(E, Store), %% resolve entry point
    Store1 = dict:store(FA, PosE,Store),
    load_code(Code,Pos,LCode,Store1).

resolve_code([{call,A,{_M,F,A}}|Code], Acc, Store) ->
    {ok,Pos} = dict:find({F,A}, Store),
    resolve_code(Code, [{call,[A,{f,Pos}]}|Acc], Store);
resolve_code([{call_last,A,{_M,F,A},U}|Code], Acc, Store) ->
    {ok,Pos} = dict:find({F,A}, Store),
    resolve_code(Code, [{call_last,[A,{f,Pos},U]}|Acc], Store);
resolve_code([{call_only,A,{_M,F,A}}|Code], Acc, Store) ->
    {ok,Pos} = dict:find({F,A}, Store),
    resolve_code(Code, [{call_only,[A,{f,Pos}]}|Acc], Store);
resolve_code([I|Code], Acc, Store) ->
    I1 =
        case I of
            %% undo some of the beam_disasm work
            {arithfbif,Op,F,[A1,A2],Reg} ->
                {Op,[resolve_arg(F,Store),A1,A2,Reg]};
            {arithfbif,Op,F,[A1],Reg} ->
                {Op,[resolve_arg(F,Store),A1,Reg]};
            {arithbif,Op,F,[A1,A2],Reg} ->
                case Op of
                    '+' -> {m_plus,[resolve_arg(F,Store),A1,A2,Reg]};
                    '-' -> {m_minus,[resolve_arg(F,Store),A1,A2,Reg]};
                    '*' -> {m_times,[resolve_arg(F,Store),A1,A2,Reg]};
                    '/' -> {m_div,[resolve_arg(F,Store),A1,A2,Reg]};
                    'div' -> {int_div,[resolve_arg(F,Store),A1,A2,Reg]};
                    'rem' -> {int_rem,[resolve_arg(F,Store),A1,A2,Reg]};
                    'band' -> {int_band,[resolve_arg(F,Store),A1,A2,Reg]};
                    'bor' -> {int_bor,[resolve_arg(F,Store),A1,A2,Reg]};
                    'bxor' -> {int_bxor,[resolve_arg(F,Store),A1,A2,Reg]};
                    'bsl' -> {int_bsl,[resolve_arg(F,Store),A1,A2,Reg]};
                    'bsr' -> {int_bsr,[resolve_arg(F,Store),A1,A2,Reg]}
                end;
            {arithbif,Op,F,[A1],Reg} ->
                case Op of
                    'bnot' -> {int_bnot,[resolve_arg(F,Store),A1,Reg]}
                end;

            {test,Test,F,Args} ->
                {Test, [resolve_arg(F,Store)|Args]};

            _ when is_atom(I) ->
                {I,[]};
            _ when is_tuple(I) ->
                [Op|Args] = tuple_to_list(I),
                {Op,resolve_args(Args,Store)}
        end,
    resolve_code(Code, [I1|Acc], Store);
resolve_code([], Acc, Store) ->
    {ok, {list_to_tuple(Acc), Store}}.

resolve_arg(Arg, Store) ->
    case Arg of
        {f,0} -> Arg;
        {f,L} ->
            case dict:find(L, Store) of
                {ok,Pos} -> {f,Pos};
                error  ->
                    io:format("unable to lookup label ~p\n", [L]),
                    {f,undefined}
            end;
        {list,L} ->
            L1 = resolve_args(L,Store),
            {list,L1};
        _ ->
            Arg
    end.

resolve_args([A|As],Store) ->
    [resolve_arg(A,Store) | resolve_args(As,Store)];
resolve_args([],_Store) ->
    [].


get_section(Name, Sections, Default) ->
    case lists:keysearch(Name,1,Sections) of
        false -> Default;
        {value, {_, Value}} -> Value
    end.

get_section(Name, Sections) ->
    case lists:keysearch(Name,1,Sections) of
        {value, {_, Value}} -> Value
    end.











