%%% File    : beam_emu.erl
%%% Author  : Tony Rogvall <tony@iMac.local>
%%% Description : BEAM interpreter
%%% Created : 10 Jan 2006 by Tony Rogvall <tony@iMac.local>

-module(prototype_emu).

%%
%% Simple API to call a emulated function
%%
-export([run/3]).

%% Internal API to discover the meta level ;-)
-export([level/0, level0/0]).

%%
%% Instruction set. The first argument is the Beam Instruction State
%% then the instruction arguments (so -1 on arity to the orgininal)
%%
-export([label/2]).              %% 1
-export([func_info/4]).          %% 2
-export([int_code_end/1]).       %% 3
-export([call/3]).               %% 4
-export([call_last/4]).          %% 5
-export([call_only/3]).          %% 6
-export([call_ext/3]).           %% 7
-export([call_ext_last/4]).      %% 8
-export([bif0/4]).               %% 9
-export([bif1/5]).               %% 10
-export([bif2/6]).               %% 11
-export([allocate/3]).           %% 12
-export([allocate_heap/4]).      %% 13
-export([allocate_zero/3]).      %% 14
-export([allocate_heap_zero/4]). %% 15
-export([test_heap/3]).          %% 16
-export([init/2]).               %% 17
-export([deallocate/2]).         %% 18
-export([return/1]).             %% 19
-export([send/1]).               %% 20
-export([remove_message/1]).     %% 21
-export([timeout/1]).            %% 22
-export([loop_rec/3]).           %% 23
-export([loop_rec_end/2]).       %% 24
-export([wait/2]).               %% 25
-export([wait_timeout/3]).       %% 26
-export([m_plus/5]).             %% 27*
-export([m_minus/5]).            %% 28*
-export([m_times/5]).            %% 29*
-export([m_div/5]).              %% 30*
-export([int_div/5]).            %% 31*
-export([int_rem/5]).            %% 32*
-export([int_band/5]).           %% 33*
-export([int_bor/5]).            %% 34*
-export([int_bxor/5]).           %% 35*
-export([int_bsl/5]).            %% 36*
-export([int_bsr/5]).            %% 37*
-export([int_bnot/4]).           %% 38*
-export([is_lt/4]).              %% 39
-export([is_ge/4]).              %% 40
-export([is_eq/4]).              %% 41
-export([is_ne/4]).              %% 42
-export([is_eq_exact/4]).        %% 43
-export([is_ne_exact/4]).        %% 44
-export([is_integer/3]).         %% 45
-export([is_float/3]).           %% 46
-export([is_number/3]).          %% 47
-export([is_atom/3]).            %% 48
-export([is_pid/3]).             %% 49
-export([is_reference/3]).       %% 50
-export([is_port/3]).            %% 51
-export([is_nil/3]).             %% 52
-export([is_binary/3]).          %% 53
-export([is_constant/3]).        %% 54*
-export([is_list/3]).            %% 55
-export([is_nonempty_list/3]).   %% 56
-export([is_tuple/3]).           %% 57
-export([test_arity/4]).         %% 58
-export([select_val/4]).         %% 59
-export([select_tuple_arity/4]). %% 60
-export([jump/2]).               %% 61
-export(['catch'/3]).            %% 62
-export([catch_end/2]).          %% 63
-export([move/3]).               %% 64
-export([get_list/4]).           %% 65
-export([get_tuple_element/4]).  %% 66
-export([set_tuple_element/4]).  %% 67
-export([put_string/4]).         %% 68*
-export([put_list/4]).           %% 69
-export([put_tuple/3]).          %% 70
-export([put/2]).                %% 71
-export([badmatch/2]).           %% 72
-export([if_end/1]).             %% 73
-export([case_end/2]).           %% 74
-export([call_fun/2]).           %% 75
-export([make_fun/4]).           %% 76*
-export([is_function/3]).        %% 77
-export([call_ext_only/3]).      %% 78
-export([bs_start_match/3]).     %% 79*
-export([bs_get_integer/3]).     %% 80*
-export([bs_get_float/3]).       %% 81*
-export([bs_get_binary/3]).      %% 82*
-export([bs_skip_bits/3]).       %% 83*
-export([bs_test_tail/3]).       %% 84*
-export([bs_save/2]).            %% 85*
-export([bs_restore/2]).         %% 86*
-export([bs_init/3]).            %% 87*
-export([bs_final/3]).           %% 88*
-export([bs_put_integer/6]).     %% 89
-export([bs_put_binary/6]).      %% 90
-export([bs_put_float/6]).       %% 91
-export([bs_put_string/3]).      %% 92
-export([bs_need_buf/2]).        %% 93*
-export([fclearerror/1]).        %% 94
-export([fcheckerror/2]).        %% 95
-export([fmove/3]).              %% 96
-export([fconv/3]).              %% 97
-export([fadd/5]).               %% 98
-export([fsub/5]).               %% 99
-export([fmul/5]).               %% 100
-export([fdiv/5]).               %% 101
-export([fnegate/4]).            %% 102
-export([make_fun2/5]).          %% 103
-export(['try'/3]).              %% 104*
-export([try_end/2]).            %% 105*
-export([try_case/2]).           %% 106*
-export([try_case_end/2]).       %% 107*
-export([raise/3]).              %% 108*
-export([bs_init2/7]).           %% 109
-export([bs_bits_to_bytes/4]).   %% 110*
-export([bs_add/4]).             %% 111
-export([apply/2]).              %% 112
-export([apply_last/3]).         %% 113
-export([is_boolean/3]).         %% 114
-export([is_function2/4]).       %% 115
-export([bs_start_match2/6]).    %% 116*
-export([bs_get_integer2/8]).    %% 117*
-export([bs_get_float2/8]).      %% 118*
-export([bs_get_binary2/8]).     %% 119*
-export([bs_skip_bits2/6]).      %% 120*
-export([bs_test_tail2/4]).      %% 121*
-export([bs_save2/3]).           %% 122*
-export([bs_restore2/3]).        %% 123*
-export([gc_bif1/6]).            %% 124
-export([gc_bif2/7]).            %% 125
-export([bs_final2/3]).          %% 126*
-export([bs_bits_to_bytes2/3]).  %% 127*
-export([put_literal/3]).        %% 128*
-export([is_bitstr/3]).          %% 129
-export([bs_context_to_binary/2]). %% 130
-export([bs_test_unit/4]).       %% 131
-export([bs_match_string/5]).    %% 132
-export([bs_init_writable/1]).   %% 133
-export([bs_append/9]).          %% 134
-export([bs_private_append/7]).  %% 135
-export([trim/3]).               %% 136
-export([bs_init_bits/7]).       %% 137
-export([bs_get_utf8/6]).        %% 138
-export([bs_skip_utf8/5]).       %% 139
-export([bs_get_utf16/6]).       %% 140
-export([bs_skip_utf16/5]).      %% 141
-export([bs_get_utf32/6]).       %% 142
-export([bs_skip_utf32/5]).      %% 143
-export([bs_utf8_size/4]).       %% 144
-export([bs_put_utf8/4]).        %% 145
-export([bs_utf16_size/4]).      %% 146
-export([bs_put_utf16/4]).       %% 147
-export([bs_put_utf32/4]).       %% 148
-export([on_load/1]).            %% 149
-export([recv_mark/2]).          %% 150
-export([recv_set/2]).           %% 151
-export([gc_bif3/8]).            %% 152
-export([line/2]).               %% 153
-export([put_map_assoc/6]).      %% 154*
-export([put_map_exact/6]).      %% 155*
-export([is_map/3]).             %% 156*
-export([has_map_field/4]).      %% 157*
-export([get_map_element/5]).    %% 158*

%% generated from beam_load
-export([bif/5]).
-export([gc_bif/6]).

-import(lists, [map/2, foldr/3, reverse/1]).

-define(BLANK, []).
-define(THE_NON_VALUE(S), S#s.non).
-define(is_non_value(X,S), (?THE_NON_VALUE(S) =:= X)).

-define(BSF_ALIGNED, 1).
-define(BSF_LITTLE,  2).
-define(BSF_SIGNED,  4).
-define(BSF_EXACT,   8).
-define(BSF_NATIVE,  16).

%%
%% @type beam_state() = term()
%% @type arity() = non_negative_integer()
%% @type label() = {'f',non_negative_integer()}
%% @type void() = term()
%% @type register() = term()
%%
-record(s,
        {  x,
           y,
           f,
           cp,       %% continuation pointer
           i,        %% instruction pointer
           code,     %% current code block
           cs=[],    %% catch stack
           ferror,   %% floating point error code...
           timer,    %% timeout timer
           mark,     %% message mark
           saved,    %% message saved position
           tuple_dst,
           tuple_arity = 0,
           tuple_data = [],
           non,         %% non value
           br,          %% destination bit/binary reg
           bb = <<>>    %% binary buffer
          }).


%% @doc opcode=1
%%   End interpreted code
%%
label(S,L) ->
    io:format("meta op: label ~w\n",[L]),  %% exit?
    next(S).

%%
%% @spec func_info(_S::beam_state(),Module::atom(),F::atom(),
%%                 Arity::non_neg_integer()) -> void()
%%
%% @doc opcode=2
%%   Function information block
%%
func_info(S, M,F,Arity) ->
    fail(S#s { i=S#s.i+2},{f,0},error,{function_clause,M,F,Arity}).
%%
%% @spec int_code_end(_S::beam_state())  -> void()
%%
%% @doc opcode=3
%%   End interpreted code
%%
int_code_end(S) ->
    io:format("meta op: int_code_end\n"),  %% exit?
    next(S).

%%
%% @spec call(_S::beam_state(), Arity::arity(),Label::label()) -> void()
%%
%% @doc opcode=4
%%
call(S, _Arity, {f,I}) ->
    Ys = [S#s.i+1 | S#s.y],
    dispatch(S#s { i=I, y=Ys}).

%%
%% @spec call_last(_S::beam_state,Arity::arity(),Label::label(),Dealloc::non_negative_integer()) -> void()
%%
%% @doc opcode=5
%%
call_last(S, _Arity, {f,I}, Dealloc) ->
    Ys = deallocate_(Dealloc, S#s.y),
    dispatch(S#s { i=I, y=Ys}).

%%
%% @doc opcode=6
%%
call_only(S, _Arity,{f,I1}) ->
    dispatch(S#s { i=I1}).

%%
%% @doc opcode=7
%% @todo
%%   Check if the module is beam interpreted then
%%   and pass context, push code together with the return position.
%%
call_ext(S, Arity,{extfunc,Mod,Fun,Arity}) ->
    call_ext_(S, Mod,Fun,Arity).

%%
%% @doc opcode=8
%%
call_ext_last(S, Arity,{extfunc,Mod,Fun,Arity},Dealloc) ->
    call_ext_last_(S, Mod,Fun,Arity,Dealloc).

%%
%% @doc opcode=9
%%
bif0(S, Bif, nofail, Dst) ->
    Val = apply(erlang,Bif,[]),
    next(store(Dst,Val,S)).

%%
%% @doc opcode=10
%%
bif1(S, Bif, Fail, A1, Dst) ->
    case catch apply(erlang,Bif,[fetch(A1,S)]) of
        {'EXIT',Reason} ->
            fail(S, Fail,exit,Reason);
        Val ->
            next(store(Dst,Val,S))
    end.

%%
%% @doc opcode=11
%%
bif2(S, Bif, Fail, A1, A2, Dst) ->
    case catch apply(erlang,Bif,[fetch(A1,S),fetch(A2,S)]) of
        {'EXIT',Reason} ->
            fail(S, Fail,exit,Reason);
        Val ->
            next(store(Dst,Val,S))
    end.

%% @hidden
%%  entry point from beam_load (maybe translate)
bif(S,Bif,Fail,[],Dst) ->  bif0(S,Bif,Fail,Dst);
bif(S,Bif,Fail,[A1],Dst) -> bif1(S,Bif,Fail,A1,Dst);
bif(S,Bif,Fail,[A1,A2],Dst) -> bif2(S,Bif,Fail,A1,A2,Dst).

%%
%% @doc opcode=12
%%
allocate(S, StackNeed,_Live) ->
    %% FIXME: maybe kill some regs
    Ys = allocate_(StackNeed,[],S#s.y),
    next(S#s { y = Ys }).

%%
%% @doc opcode=13
%%
allocate_heap(S, StackNeed,_HeapNeed,_Live) ->
    %% FIXME: maybe kill some regs
    Ys = allocate_(StackNeed,[],S#s.y),
    next(S#s { y = Ys }).

%%
%% @doc opcode=14
%%
allocate_zero(S,StackNeed,_Live) ->
    %% FIXME: maybe kill some regs
    Ys = allocate_(StackNeed,?BLANK,S#s.y),
    next(S#s { y = Ys }).

%% @doc opcode=15
allocate_heap_zero(S,StackNeed,_HeapNeed,_Live) ->
    %% FIXME: maybe kill some regs
    Ys = allocate_(StackNeed,?BLANK,S#s.y),
    next(S#s { y = Ys }).

%% @doc opcode=16
test_heap(S,{alloc,[{words,_N},{floats,_F}]},_Live) ->
    %% FIXME: emulate this better
    %% heap and float are dynamic
    next(S);
%%
%% @doc opcode=16
%%
test_heap(S,_HeapNeed,_Live) ->
    %% FIXME: emulate this better
    %% heap is dynamic
    next(S).
%%
%% @doc opcode=17
%%
init(S,Dst) ->
    next(make_blank(Dst,S)).
%%
%% @doc opcode=18
%%
deallocate(S,Deallocate) ->
    Ys = deallocate_(Deallocate, S#s.y),
    next(S#s { y = Ys }).

%%
%% @doc opcode=19
%%   return value
%% @todo check if IRet is on form {Pos,Code} then install the code!
%%
return(S) ->
    case S#s.y of
        [IRet|Ys] ->
            dispatch(S#s {i=IRet, y=Ys});
        [] ->
            fetch({x,0},S)
    end.

%%
%% @doc opcode=20
%%
send(S) ->
    Result = (fetch({x,0},S) ! fetch({x,1},S)),
    S1 = store({x,0}, Result, S),
    next(S1).

%%
%% @doc opcode=21
%%
remove_message(S) ->
    message:remove(),
    next(S).

%%
%% @doc opcode=22
%%
timeout(S) ->
    message:first(), %% restart scanning
    next(S).

%%
%% @doc opcode=23
%%
loop_rec(S,{f,IL},Dst) ->
    case message:current() of
        empty ->
            %% jump to wait or wait_timeout
            dispatch(S#s { i=IL});
        {message,M} ->
            next(store(Dst,M,S))
    end.

%%
%% @doc opcode=24
%%
loop_rec_end(S,{f,IL}) ->
    _Ignore = message:next(),
    dispatch(S#s {i=IL}).

%%
%% @doc opcode=25
%%
wait(S,{f,IL}) ->
    message:next(infinity),
    next(S#s {i=IL}).

%%
%% @doc opcode=26
%%
wait_timeout(S,{f,IL},Src) ->
    case S#s.timer of
        undefined ->
            Tmo = fetch(Src,S),
            if Tmo == infinity ->
                    message:next(),
                    dispatch(S#s {i=IL});
               Tmo >= 0, Tmo =< 16#ffffffff ->
                    Timer = erlang:start_timer(Tmo,undefined,tmo),
                    case message:next(Tmo) of
                        timeout ->
                            next(S);
                        {message,_} ->
                            dispatch(S#s{i=IL,timer=Timer})
                    end;
               true ->
                    fail(S,{f,0},error,timeout_value)
            end;
        Timer ->
            Timeout = case erlang:read_timer(Timer) of
                          false -> 0;
                          RVal -> RVal
                      end,
            case message:next(Timeout) of
                timeout ->
                    next(S#s { timer=undefined });
                {message,_} ->
                    dispatch(S#s {i=IL})
            end
    end.

%%
%% 27...38 not generated by compiler anymore? (but may be expanded?)

%%
%% @doc opcode=27
%%
m_plus(S,Fail,A1,A2,Reg) ->
    binary_op(S,Fail,A1,A2,Reg,fun(A,B) -> A+B end).

%%
%% @doc opcode=28
%%
m_minus(S,Fail,A1,A2,Reg) ->
    binary_op(S,Fail,A1,A2,Reg,fun(A,B) -> A-B end).

%%
%% @doc opcode=29
%%
m_times(S,Fail,A1,A2,Reg) ->
    binary_op(S,Fail,A1,A2,Reg,fun(A,B) -> A*B end).

%%
%% @doc opcode=30
%%
m_div(S,Fail,A1,A2,Reg) ->
    binary_op(S,Fail,A1,A2,Reg,fun(A,B) -> A / B end).

%%
%% @doc opcode=31
%%
int_div(S,Fail,A1,A2,Reg) ->
    binary_op(S,Fail,A1,A2,Reg,fun(A,B) -> A div B end).

%%
%% @doc opcode=32
%%
int_rem(S,Fail,A1,A2,Reg) ->
    binary_op(S,Fail,A1,A2,Reg,fun(A,B) -> A rem B end).

%%
%% @doc opcode=33
%%
int_band(S,Fail,A1,A2,Reg) ->
    binary_op(S,Fail,A1,A2,Reg,fun(A,B) -> A band B end).

%%
%% @doc opcode=34
%%
int_bor(S,Fail,A1,A2,Reg) ->
    binary_op(S,Fail,A1,A2,Reg,fun(A,B) -> A bor B end).

%%
%% @doc opcode=35
%%
int_bxor(S,Fail,A1,A2,Reg) ->
    binary_op(S,Fail,A1,A2,Reg,fun(A,B) -> A bxor B end).

%%
%% @doc opcode=36
%%
int_bsl(S,Fail,A1,A2,Reg) ->
    binary_op(S,Fail,A1,A2,Reg,fun(A,B) -> A bsl B end).

%%
%% @doc opcode=37
%%
int_bsr(S,Fail,A1,A2,Reg) ->
    binary_op(S,Fail,A1,A2,Reg,fun(A,B) -> A bsr B end).

%%
%% @doc opcode=38
%%
int_bnot(S,Fail,A1,Reg) ->
    unary_op(S,Fail,A1,Reg,fun(A) -> bnot A end).

%%
%% @doc opcode=39
%%
is_lt(S,Fail,A1,A2) ->
    compare_op(S,Fail,A1,A2,fun(A,B) -> A < B end).


%%
%% @doc opcode=40
%%
is_ge(S,Fail,A1,A2) ->
    compare_op(S,Fail,A1,A2,fun(A,B) -> A >= B end).

%%
%% @doc opcode=41
%%
is_eq(S,Fail,A1,A2) ->
    compare_op(S,Fail,A1,A2,fun(A,B) -> A == B end).

%%
%% @doc opcode=42
%%
is_ne(S,Fail,A1,A2) ->
    compare_op(S,Fail,A1,A2,fun(A,B) -> A /= B end).

%%
%% @doc opcode=43
%%
is_eq_exact(S,Fail,A1,A2) ->
    compare_op(S,Fail,A1,A2,fun(A,B) -> A =:= B end).

%%
%% @doc opcode=44
%%
is_ne_exact(S,Fail,A1,A2) ->
    compare_op(S,Fail,A1,A2,fun(A,B) -> A =/= B end).

%%
%% @doc opcode=45
%%
is_integer(S,Fail,A1) ->
    test_op(S,Fail,A1,fun(A) -> is_integer(A) end).

%%
%% @doc opcode=46
%%
is_float(S,Fail,A1) ->
    test_op(S,Fail,A1,fun(A) -> is_float(A) end).

%% @doc opcode=47
is_number(S,Fail,A1) ->
    test_op(S,Fail,A1,fun(A) -> is_number(A) end).


%% @doc opcode=48
is_atom(S,Fail,A1) ->
    test_op(S,Fail,A1,fun(A) -> is_atom(A) end).

%% @doc opcode=49
is_pid(S,Fail,A1) ->
    test_op(S,Fail,A1,fun(A) -> is_pid(A) end).

%% @doc opcode=50
is_reference(S,Fail,A1) ->
    test_op(S,Fail,A1,fun(A) -> is_reference(A) end).

%% @doc opcode=51
is_port(S,Fail,A1) ->
    test_op(S,Fail,A1,fun(A) -> is_port(A) end).


%% @doc opcode=52
is_nil(S,Fail,A1) ->
    test_op(S,Fail,A1,fun(A) -> A =:= [] end).

%% @doc opcode=53
is_binary(S,Fail,A1) ->
    test_op(S,Fail,A1,fun(A) -> is_binary(A) end).

%% @doc opcode=54
is_constant(_S,_Fail,_A1) ->
    {not_implemented, 54}.

%% @doc opcode=55
is_list(S,Fail,A1) ->
    test_op(S,Fail,A1,fun(A) -> is_list(A) end).

%% @doc opcode=56
is_nonempty_list(S,Fail,A1) ->
    test_op(S,Fail,A1,fun([_|_]) -> true;
                         (_) -> false end).
%%
%% @doc opcode=57
%%
is_tuple(S,Fail,A1) ->
    case is_tuple(fetch(A1,S)) of
        false -> fail(S,Fail);
        true -> next(S)
    end.

%% @doc opcode=58
%% @todo check size arg!
test_arity(S,Fail,Src,Size) ->
    Val = fetch(Src,S),
    if is_tuple(Val), size(Val) == Size ->
            next(S);
       true -> fail(S,Fail)
    end.

%%
%% @doc opcode=59
%%
select_val(S,Val,Fail,{list,Pairs}) ->
    case select_val(fetch(Val,S), Pairs) of
        {f,I1} ->
            dispatch(S#s{i=I1});
        false ->
            fail(S,Fail)
    end.

%%
%% @doc opcode=60
%%
select_tuple_arity(S,Val,Fail,{list,Pairs}) ->
    T = fetch(Val, S),
    if is_tuple(T) ->
            case select_val(size(T), Pairs) of
                {f,I1} ->
                    dispatch(S#s {i=I1});
                false ->
                    fail(S,Fail)
            end;
       true ->
            fail(S,Fail)
    end.

%%
%% @doc opcode=61
%%
jump(S,{f,I1}) ->
    dispatch(S#s{i=I1}).

%%
%% @spec catch(_S::beam_state(),Dst::register(),Fail::label()) -> void()
%%
%% @doc opcode=62
%%

'catch'(S,Dst,Fail) ->
    S1 = store(Dst,Fail,S),  %% just for the record
    Cs = [{Fail,length(S1#s.y)} | S1#s.cs],
    next(S1#s { cs = Cs }).

%%
%% @doc opcode=63
%%
catch_end(S,Dst) ->
        S1 = make_blank(Dst,S),  %% just for the record
        Cs = tl(S1#s.cs),
        X0 = fetch({x,0},S1),
        S2 = if ?is_non_value(X0,S) ->
                     X1 = fetch({x,1},S1),
                     X2 = fetch({x,2},S1),
                     if X1 == thrown ->
                             store({x,0},X2,S1);
                        X1 == error ->
                             store({x,0},{X2,[stack_trace]},S1);
                        true ->
                             store({x,0},{'EXIT',X2},S1)
                     end;
                true ->
                     S1
             end,
        next(S2#s { cs = Cs }).

%%
%% @doc opcode=64
%%
move(S,Src,Dst) ->
    S1 = store(Dst, fetch(Src,S), S),
    next(S1).

%%
%% @doc opcode=65
%%
get_list(S,Src,Head,Tail) ->
    [H|T] = fetch(Src,S),
    S1 = store(Head,H,S),
    S2 = store(Tail,T,S1),
    next(S2).

%%
%% @doc opcode=66
%%
get_tuple_element(S,Src,Ix,Dst) ->
    E = element(Ix+1, fetch(Src,S)),
    next(store(Dst,E,S)).

%%
%% @doc opcode=67
%%
set_tuple_element(S,Val,Dst,Ix) ->
    T = setelement(Ix,fetch(Dst,S),fetch(Val,S)),
    next(store(Dst,T,S)).

%%
%% @doc opcode=68
%%
put_string(S,_Len,{string,String},Dst) ->
    next(store(Dst,String,S)).

%%
%% @doc opcode=69
%%
put_list(S,Head,Tail,Dst) ->
    L = [fetch(Head,S) | fetch(Tail,S)],
    next(store(Dst,L,S)).

%%
%% @doc opcode=70
%%
put_tuple(S,Arity,Dst) ->
    S1 =
        if Arity == 0 ->
                store(Dst,{},S);
           true ->
                S#s { tuple_dst=Dst, tuple_arity=Arity, tuple_data=[]}
        end,
    next(S1).

%%
%% @doc opcode=71
%%
put(S,Src) ->
    Val = fetch(Src,S),
    Data = [Val | S#s.tuple_data],
    S1 =
        case S#s.tuple_arity-1 of
            0 ->
                store(S#s.tuple_dst, list_to_tuple(reverse(Data)),
                      S#s { tuple_dst=undefined, tuple_arity=0, tuple_data=[]});
            A ->
                S#s { tuple_arity=A, tuple_data=Data }
        end,
    next(S1).

%%
%% @doc opcode=72
%% @todo Check this
%%
badmatch(S,Fail) ->
    fail(S,Fail,error,badmatch).
%%
%% @doc opcode=73
%% @todo I++ ?
%%
if_end(S) ->
    fail(S,{f,0},error,if_clause).

%%
%% @doc opcode=74
%%
case_end(S,CaseVal) ->
    fail(S,{f,0},error,{case_clause,fetch(CaseVal,S)}).

%%
%% @doc opcode=75
%%
call_fun(S,Arity) ->
  As = fetch_regs(Arity, S),
  Fun = fetch({x, Arity}, S),
  Ret0 = case Fun of
           {funobject, #{mfarity := MFArity}} ->
             {M, F, Arity} = MFArity,
             Ys = [S#s.i+1 | S#s.y], %push local address
             dispatch(S#s { i=I, y=Ys});
           _ -> catch erlang:apply(Fun, As)
         end,
  case Ret0 of
    {'EXIT', Reason} ->
      fail(S, {f, 0}, exit, Reason);
    Ret ->
      next(store({x, 0}, Ret, S))
  end.

%%
%% @doc opcode=76
%%
make_fun(_S, _Arg1, _Arg2, _Arg3) ->
    {not_implemented, 76}.

%%
%% @doc opcode=77
%%
is_function(S,Fail,A1) ->
    case is_function(fetch(A1,S)) of
        false -> fail(S,Fail);
        true -> next(S)
    end.

%%
%% @doc opcode=78
%%
call_ext_only(S,Arity,{extfunc,Mod,Fun,Arity}) ->
    call_ext_last_(S,Mod,Fun,Arity,0).

%% opcodes 79..88 not generated

%%
%% @doc opcode=79
%%
bs_start_match(_S,_Fail,_Reg) ->
    {not_implemented,79}.
%%
%% @doc opcode=80
%%
bs_get_integer(_S,_Fail,[_Arg,_N,_Flags,_Dst]) ->
    {not_implemented,80}.
%%
%% @doc opcode=81
%%
bs_get_float(_S,_Fail,[_Arg,_N,_Flags,_Dst]) ->
    {not_implemented,81}.
%%
%% @doc opcode=82
%%
bs_get_binary(_S,_Fail,[_Arg,_N,_Flags,_Dst]) ->
    {not_implemented,82}.
%%
%% @doc opcode=83
%%
bs_skip_bits(_S,_Fail,[_Arg,_N,_Flags]) ->
    {not_implemented,83}.
%%
%% @doc opcode=84
%%
bs_test_tail(_S,_Fail,[_N]) ->
    {not_implemented,84}.
%%
%% @doc opcode=85
%%
bs_save(_S,_N) ->
    {not_implemented,85}.
%%
%% @doc opcode=86
%%
bs_restore(_S,_N) ->
    {not_implemented,86}.
%%
%% @doc opcode=87
%%
bs_init(_S,_N,_Flags) ->
    {not_implemented,87}.
%%
%% @doc opcode=88
%%
bs_final(_S,_Fail,_X) ->
    {not_implemented,88}.
%%
%% @doc opcode=89
%%
bs_put_integer(S,Fail,ArgSz,_N,{field_flags,Flags},ArgInt) ->
    Int = fetch(ArgInt, S),
    Sz  = fetch(ArgSz, S),
    case catch (if Flags band ?BSF_LITTLE =/= 0 ->
                        <<(S#s.bb)/bits, Int:Sz/little>>;
                   Flags band ?BSF_NATIVE =/= 0 ->
                        <<(S#s.bb)/bits, Int:Sz/native>>;
                   true ->
                        <<(S#s.bb)/bits, Int:Sz>>
                end) of
               {'EXIT',Reason} ->
                 fail(S,Fail,exit,Reason);
               BB ->
                 S1 = store(S#s.br, BB, S),
                 S2 = S1#s { bb = BB },
                 next(S2)
         end.

%%
%% @doc opcode=90
%%
bs_put_binary(S,Fail,ArgSz,_N,{field_flags,Flags},ArgBin) ->
    Bin = fetch(ArgBin, S),
    Sz  = fetch(ArgSz, S),
    case catch (if Flags band ?BSF_LITTLE =/= 0 ->
                        <<(S#s.bb)/bits, Bin:Sz/binary-little>>;
                   Flags band ?BSF_NATIVE =/= 0 ->
                        <<(S#s.bb)/bits, Bin:Sz/binary-native>>;
                   true ->
                        <<(S#s.bb)/bits, Bin:Sz/binary>>
                end) of
               {'EXIT',Reason} ->
                 fail(S,Fail,exit,Reason);
               BB ->
                 S1 = store(S#s.br, BB, S),
                 S2 = S1#s { bb = BB },
                 next(S2)
         end.

%%
%% @doc opcode=91
%%
bs_put_float(S,Fail,ArgSz,_N,{field_flags,Flags},ArgFloat) ->
    Flt = fetch(ArgFloat, S),
    Sz  = fetch(ArgSz, S),
    case catch (if Flags band ?BSF_LITTLE =/= 0 ->
                        <<(S#s.bb)/bits, Flt:Sz/little-float>>;
                   Flags band ?BSF_NATIVE =/= 0 ->
                        <<(S#s.bb)/bits, Flt:Sz/native-float>>;
                   true ->
                        <<(S#s.bb)/bits, Flt:Sz/float>>
                end) of
               {'EXIT',Reason} ->
                 fail(S,Fail,exit,Reason);
               BB ->
                 S1 = store(S#s.br, BB, S),
                 S2 = S1#s { bb = BB },
                 next(S2)
         end.

%%
%% @doc opcode=92
%%
bs_put_string(S,_Len,StrArg) ->
    String = fetch(StrArg, S),
    BB = <<(S#s.bb)/bits,(list_to_binary(String))/bits>>,
    S1 = store(S#s.br, BB, S),
    S2 = S1#s { bb = BB },
    next(S2).

%%
%% @doc opcode=93
%%
bs_need_buf(_S,_N) ->
    {not_implemented,93}.

%%
%% @doc opcode=94
%%
fclearerror(S) ->
    next(S#s { ferror=undefined}).

%% @doc opcode=95
fcheckerror(S,Fail) ->
    if S#s.ferror == undefined ->
            next(S);
       true ->
            fail(S,Fail,error,S#s.ferror)
    end.

%% @doc opcode=96
fmove(S,Src,FDst) ->
    S1 = store(FDst, fetch(Src,S), S),
    next(S1).

%% @doc opcode=97
%% @todo Check the bignum conversion to float
fconv(S,Src,FDst) ->
    case fetch(Src, S) of
        Int when is_integer(Int) ->
            next(store(FDst,float(Int),S));
        Float when is_float(Float) ->
            next(store(FDst,Float,S));
        _ ->
            fail(S,{f,0},error,badarith)
    end.

%% @doc opcode=98
fadd(S,_Fail,FA1,FA2,FDst) ->
    case catch (fetch(FA1,S) + fetch(FA2,S)) of
        {'EXIT',Reason} ->
            next(S#s { ferror=Reason});
        Val ->
            next(store(FDst,Val,S))
    end.

%% @doc opcode=99
fsub(S,_Fail,FA1,FA2,FDst) ->
    case catch (fetch(FA1,S) - fetch(FA2,S)) of
        {'EXIT',Reason} ->
            next(S#s { ferror=Reason});
        Val ->
            next(store(FDst,Val,S))
    end.

%% @doc opcode=100
fmul(S,_Fail,FA1,FA2,FDst) ->
    case catch (fetch(FA1,S) * fetch(FA2,S)) of
        {'EXIT',Reason} ->
            next(S#s { ferror=Reason });
        Val ->
            next(store(FDst,Val,S))
    end.

%% @doc opcode=101
fdiv(S,_Fail,FA1,FA2,FDst) ->
    case catch (fetch(FA1,S) / fetch(FA2,S)) of
        {'EXIT',Reason} ->
            next(S#s { ferror=Reason});
        Val ->
            next(store(FDst,Val,S))
    end.

%% @doc opcode=102
fnegate(S,_Fail,FA1,FDst) ->
    case catch -fetch(FA1,S) of
        {'EXIT',Reason} ->
            next(S#s { ferror=Reason});
        Val ->
            next(store(FDst,Val,S))
    end.

%% @doc opcode=103
make_fun2(S, Loc, _P1, _P2, _P3) ->
  Fun = prototype_fun:new(Loc),
  S1 = store({x,0}, Fun, S),
  next(S1).
  %{not_implemented,103}.

%% @doc opcode=104
'try'(_S,_Reg,_Fail) ->
    {not_implemented,104}.

%% @doc opcode=105
try_end(S,Reg) ->
    S1 = make_blank(Reg,S),  %% just for the record
    Cs = tl(S1#s.cs),
    X0 = fetch({x,0},S1),
    if ?is_non_value(X0,S1) ->
            S2 = store({x,0}, fetch({x,1},S1), S1),
            S3 = store({x,1}, fetch({x,2},S1), S2),
            S4 = store({x,2}, fetch({x,3},S1), S3),
            next(S4#s { cs = Cs });
       true ->
            next(S1#s { cs = Cs })
    end.

%% @doc opcode=106
try_case(_S,_Reg) ->
    {not_implemented,106}.

%% @doc opcode=107
try_case_end(S,TryVal) ->
    fail(S,{f,0},error,{try_clause,fetch(TryVal,S)}).

%% @doc opcode=108
raise(S,Class,Reason) ->
    %% Fixme: more work here
    fail(S,{f,0},fetch(Class,S),fetch(Reason,S)).

%% @doc opcode=109
bs_init2(S,_Fail,_Src,_W,_R,{field_flags,_Flags},Dst) ->
    S1 = S#s { br = Dst },
    next(S1).

%% @doc opcode=110
bs_bits_to_bytes(_S,_Fail,_Src,_Dst) ->
    {not_implemented,110}.

%% @doc opcode=111
bs_add(S,_Fail,[Src1,Src2,Unit],Dest) ->
    Val = fetch(Src2,S)*Unit + fetch(Src1,S),
    next(store(Dest,Val,S)).

%% @doc opcode=112
apply(S,Arity) ->
    As = fetch_regs(Arity,S),
    Mod = fetch({x,Arity},S),
    Fun = fetch({x,Arity+1},S),
    case catch apply(Mod,Fun,As) of
        {'EXIT',Reason} ->
            fail(S,{f,0},exit,Reason);
        Ret ->
            next(store({x,0},Ret,S))
    end.

%% @doc opcode=113
apply_last(S,Arity,U) ->
    As = fetch_regs(Arity,S),
    Mod = fetch({x,Arity},S),
    Fun = fetch({x,Arity+1},S),
    case catch apply(Mod,Fun,As) of
        {'EXIT',Reason} ->
            fail(S,{f,0},exit,Reason);
        Ret ->
            case deallocate_(U, S#s.y) of
                [IRet|Ys] ->
                    S1 = S#s { y=Ys },
                    S2 = store({x,0},Ret,S1),
                    dispatch(S2#s {i=IRet }); %% was S?
                [] ->
                    Ret  %% ?
            end
    end.

%%
%% @doc opcode=114
%%
is_boolean(S,Fail,A1) ->
    case is_boolean(fetch(A1,S)) of
        false -> fail(S,Fail);
        true -> next(S)
    end.
%%
%% @doc opcode=115
%%
is_function2(S,Fail,A1,A2) ->
    case is_function(fetch(A1,S),fetch(A2,S)) of
        false -> fail(S,Fail);
        true -> next(S)
    end.
%%
%% @doc opcode=116
%%
bs_start_match2(_S,_Fail,_Ctx,_Live,_Save,_Dst) ->
    {not_implemented,116}.
%%
%% @doc opcode=117
%%
bs_get_integer2(_S,_Fail,_Ctx,_Live,_Size,_N,{field_flags,_Flags},_Dst) ->
    {not_implemented,117}.
%%
%% @doc opcode=118
%%
bs_get_float2(_S,_Fail,_Ctx,_Live,_Size,_N,{field_flags,_Flags},_Dst) ->
    {not_implemented,118}.
%%
%% @doc opcode=119
%%
bs_get_binary2(_S, _Fail,_Ctx,_Live,_Size,_N,{field_flags,_Flags},_Dst) ->
    {not_implemented,119}.
%%
%% @doc opcode=120
%%
bs_skip_bits2(_S,_Fail,_CtxReg,_SizeReg,_Unit,{field_flags,0}) ->
    {not_implemented,120}.
%%
%% @doc opcode=121
%%
bs_test_tail2(_S, _Fail, _Ctx, _N) ->
    {not_implemented,121}.
%%
%% @doc opcode=122
%%
bs_save2(_S, _Ctx, _N) ->
    {not_implemented,122}.
%%
%% @doc opcode=123
%%
bs_restore2(_S, _Ctx, _N) ->
    {not_implemented, 123}.
%%
%% @doc opcode=124
%%
gc_bif1(S,Bif,Fail,_Need,A1,Dst) ->
    case catch apply(erlang,Bif,[fetch(A1,S)]) of
        {'EXIT',Reason} ->
            fail(S,Fail,exit,Reason);
        Val ->
            next(store(Dst,Val,S))
    end.

%%
%% @doc opcode=125
%%
gc_bif2(S,Bif,Fail,_Need,A1,A2,Dst) ->
    case catch apply(erlang,Bif,[fetch(A1,S),fetch(A2,S)]) of
        {'EXIT',Reason} ->
            fail(S,Fail,exit,Reason);
        Val ->
            next(store(Dst,Val,S))
    end.

%%
%% @doc opcode=126
%%
bs_final2(_S, _X, _Y) ->
    {not_implemented, 126}.
%%
%% @doc opcode=127
%%
bs_bits_to_bytes2(_S, _A2,_A3) ->
    {not_implemented, 127}.
%%
%% @doc opcode=128
%%
put_literal(_S, _Index, _Dst) ->
    {not_implemented, 128}.

%%
%% @doc opcode=129
%%
is_bitstr(S,Fail,A1) ->
    case is_bitstr(fetch(A1,S)) of
        false -> fail(S,Fail);
        true -> next(S)
    end.
%%
%% @doc opcode=130
%%
bs_context_to_binary(_S,_Dst) ->
    {not_implemented, 130}.
%%
%% @doc opcode=131
%%
bs_test_unit(_S,_Fail,_Ctx,_N) ->
    {not_implemented, 131}.
%%
%% @doc opcode=132
%%
bs_match_string(_S,_Fail,_Ctx,_Bits,_String) ->
    {not_implemented, 130}.
%%
%% @doc opcode=133
%%
bs_init_writable(_S) ->
    {not_implemented, 133}.
%%
%% @doc opcode=134
%%
bs_append(_S, _Fail, _Arg2, _W, _R, _U, _Arg6,{field_flags,_Flags},_Arg8) ->
    {not_implemented,134}.
%%
%% @doc opcode=135
%%
bs_private_append(_S, _Fail, _Arg2, _U, _Arg4, {field_flags,_Flags}, _Arg6) ->
    {not_implemented,135}.
%%
%% @doc opcode=136
%%
trim(_S,_N,_Remaining) ->
    {not_implemented,136}.
%%
%% @doc opcode=137
%%
bs_init_bits(_S,_Fail,_Arg2,_W,_R,{field_flags,_Flags},_Arg6) ->
    {not_implemented,137}.
%%
%% @doc opcode=138
%%
bs_get_utf8(_S,_Fail,_Arg2,_Arg3,{field_flags,_Flags},_Arg4) ->
    {not_implemented,138}.
%%
%% @doc opcode=139
%%
bs_skip_utf8(_S,_Fail,_Arg2,_Arg3,{field_flags,_Flags}) ->
    {not_implemented,139}.
%%
%% @doc opcode=140
%%
bs_get_utf16(_S,_Fail,_Arg2,_Arg3,{field_flags,_Flags},_Arg4) ->
    {not_implemented,140}.
%%
%% @doc opcode=141
%%
bs_skip_utf16(_S,_Fail,_Arg2,_Arg3, {field_flags,_Flags}) ->
    {not_implemented,141}.
%%
%% @doc opcode=142
%%
bs_get_utf32(_S,_Fail,_Arg2,_Arg3,{field_flags,_Flags},_Arg4) ->
    {not_implemented,142}.
%%
%% @doc opcode=143
%%
bs_skip_utf32(_S,_Fail,_Arg2,_Arg3,{field_flags,_Flags}) ->
    {not_implemented,143}.
%%
%% @doc opcode=144
%%
bs_utf8_size(_S,_Fail,_Arg2,_Arg3) ->
    {not_implemented,144}.
%%
%% @doc opcode=145
%%
bs_put_utf8(_S,_Fail,{field_flags,_Flags},_Arg3) ->
    {not_implemented,145}.
%%
%% @doc opcode=146
%%
bs_utf16_size(_S,_Fail,_Arg2,_Arg3) ->
    {not_implemented,146}.
%%
%% @doc opcode=147
%%
bs_put_utf16(_S,_Fail,{field_flags,_Flags},_Arg3) ->
    {not_implemented,147}.

%% @doc opcode=148
bs_put_utf32(_S,_Fail,{field_flags,_Flags},_Arg3) ->
    {not_implemented,148}.

%% @doc opcode=149
on_load(S) ->
    io:format("meta op: on_load\n"),  %% exit?
    next(S).

%% @doc opcode=150
recv_mark(S,{f,I}) ->
    next(S#s { mark = I, saved=message:save_message_pos() }).

%% @doc opcode=151
recv_set(S,{f,I}) ->
    if S#s.mark == I+1 ->
            message:restore_message_pos(S#s.saved);
       true ->
            ok
    end,
    next(S).

%% @doc opcode=152
gc_bif3(S,Bif,Fail,_Need,A1,A2,A3,Dst) ->
    case catch apply(erlang,Bif,[fetch(A1,S),fetch(A2,S),fetch(A3,S)]) of
        {'EXIT',Reason} ->
            fail(S,Fail,exit,Reason);
        Val ->
            next(store(Dst,Val,S))
    end.

%% @hidden dispatch from beam_load
gc_bif(S,Bif,Fail,_Need,[A1],Dst) ->
    gc_bif1(S,Bif,Fail,_Need,A1,Dst);
gc_bif(S,Bif,Fail,_Need,[A1,A2],Dst) ->
    gc_bif2(S,Bif,Fail,_Need,A1,A2,Dst);
gc_bif(S,Bif,Fail,_Need,[A1,A2,A3],Dst) ->
    gc_bif3(S,Bif,Fail,_Need,A1,A2,A3,Dst).

%% @doc opcode=153
%%      meta opcode to keep track on line number in stack traces
%% @enc
line(S, I) ->
    io:format("meta op: line ~w\n", I),
    next(S).

%% @doc opcode=154
put_map_assoc(_S,_A1,_A2,_A3,_A4,_A5) ->
    {not_implemented,154}.

%% @doc opcode=155
put_map_exact(_S,_A1,_A2,_A3,_A4,_A5) ->
    {not_implemented,155}.

%% @doc opcode=156
is_map(_S,_A1,_A2) ->
    {not_implemented,156}.

%% @doc opcode=157
has_map_field(_S,_A1,_A2,_A3) ->
    {not_implemented, 157}.

%% @doc opcode=158
get_map_element(_S,_A1,_A2,_A3,_A4) ->
    {not_implemented, 158}.

%%
%% @spec run(Module::atom(), Function::atom(), Args::[term()]) -> term()
%%
%% @doc Execute an interpreted BEAM function
%%
run(Mod, F, Args) when is_atom(Mod), is_atom(F), is_list(Args) ->
  case prototype_load:file(Mod) of
    {ok, {Mod, Exp, LCode}} ->
      A = length(Args),
      case lists:keysearch({F, A}, 1, Exp) of
        false -> erlang:error({undef, [{Mod, F, Args}]});
        {value, {_, I}} -> call_(I, LCode, Args)
      end;
    {ok, {Mod1, _Exp, _LCode}} ->
      erlang:error({load_error, Mod1});
    _Error ->
      erlang:error({load_error, Mod})
  end;
run(File, F, Args) when is_list(File), is_atom(F), is_list(Args) ->
  case prototype_load:file(File) of
    {ok, {Mod, Exp, LCode}} ->
      A = length(Args),
      case lists:keysearch({F, A}, 1, Exp) of
        false -> erlang:error({undef, [{Mod, F, Args}]});
        {value, {_, I}} -> call_(I, LCode, Args)
      end;
    _Error ->
      erlang:error({load_error, File})
  end.

%% @private
new_(Args) ->
    #s { x = list_to_tuple(Args),
         f = {0.0, 0.0},
         y = [],
         non = make_ref()    %% must be different from other terms!
       }.

%% @private
call_(I, C, Args) ->
    init_(I, C, new_(Args)).

%% @private
init_(I, C, S) ->
    dispatch(S#s { i=I, code=C }).

%% @private
next(S) ->
    dispatch(S#s { i=S#s.i + 1 }).

%% @private
%% dispatch the instruction
dispatch(S) ->
    {Op,Args} = element(S#s.i, S#s.code),
    erlang:display({level(), S#s.i, {Op,Args}}),
    apply(?MODULE, Op, [S|Args]).

%%
%% @spec level() -> non_neg_integer()
%% @doc Return the meta BEAM meta level
%%
level() ->
    prototype_emu:level0().

level0() ->
    0.

%% @private
unary_op(S,Fail,A1,Reg,Op) ->
    case catch Op(fetch(A1,S)) of
        {'EXIT',Reason} ->
            fail(S,Fail,exit,Reason);
        Val ->
            next(store(Reg,Val,S))
    end.

%% @private
binary_op(S,Fail,A1,A2,Reg,Op) ->
    case catch Op(fetch(A1,S),fetch(A2,S)) of
        {'EXIT',Reason} ->
            fail(S,Fail,exit,Reason);
        Val ->
            next(store(Reg,Val,S))
    end.

%% @private
compare_op(S,Fail,A1,A2,Op) ->
    case Op(fetch(A1,S),fetch(A2,S)) of
        false -> fail(S,Fail);
        true -> next(S)
    end.

%% @private
test_op(S,Fail,A1,Op) ->
    case Op(fetch(A1,S)) of
        false -> fail(S,Fail);
        true -> next(S)
    end.

%% @private
call_ext_(S,erlang,'throw',1) ->
    fail(S,{f,0},thrown,fetch({x,0},S));
call_ext_(S,erlang,'exit',1) ->
    fail(S,{f,0},exit,fetch({x,0},S));
call_ext_(S,beam_emu,F,0) when F==level; F==level0 ->
    Ret = level()+1,
    next(store({x,0},Ret,S));
call_ext_(S,Mod,Fun,Arity) ->
    As = fetch_regs(Arity,S),
    case catch erlang:apply(Mod,Fun,As) of
        {'EXIT',Reason} ->
            fail(S,{f,0},exit,Reason);
        Ret ->
            next(store({x,0},Ret,S))
    end.

call_ext_last_(S,erlang,'throw',1,_Dealloc) ->
    fail(S,{f,0},thrown,fetch({x,0},S));
call_ext_last_(S,erlang,'exit',1,_Dealloc) ->
    fail(S,{f,0},exit,fetch({x,0},S));
call_ext_last_(S,beam_emu,F,0,Dealloc) when  F==level; F==level0 ->
    Ret = level()+1,
    case deallocate_(Dealloc, S#s.y) of
        [IRet|Ys] ->
            S1 = S#s { y=Ys },
            S2 = store({x,0},Ret,S1),
            dispatch(S2#s { i=IRet});
        [] ->
            Ret
    end;
call_ext_last_(S,Mod,Fun,Arity,Dealloc) ->
    As = fetch_regs(Arity,S),
    case catch apply(Mod,Fun,As) of
        {'EXIT',Reason} ->
            fail(S,{f,0},exit,Reason);
        Ret ->
            case deallocate_(Dealloc, S#s.y) of
                [IRet|Ys] ->
                    S1 = S#s { y=Ys },
                    S2 = store({x,0},Ret,S1),
                    dispatch(S2#s { i=IRet });
                [] ->
                    Ret
            end
    end.

%% @private
fetch(Reg, State) ->
    Res = fetch_(Reg, State),
    io:format("fetch: ~w = ~p\n", [Reg, Res]),
    Res.

fetch_({x,I}, #s { x=X }) -> element(I+1, X);
fetch_({y,I}, #s { y=Y }) -> lists:nth(I+1,Y);
fetch_({fr,I}, #s { f=F }) -> element(I+1, F);
fetch_({atom,C},_S) -> C;
fetch_({integer,C},_S) -> C;
fetch_({float,C},_S) -> C;
fetch_(nil,_S) -> [];
fetch_({literal,Lit},_S) -> Lit;
fetch_({string,String},_S) -> String;
fetch_(Src, S) ->
    erlang:display({fetch,Src}),
    exit({function_clause,S}).

%% fetch_args([A|As], S) ->
%%     [fetch(A,S) | fetch_args(As,S)];
%% fetch_args([], _S) ->
%%    [].

%% fetch sequence of N registers {x,0} .. {x,N-1}
fetch_regs(0, _S) ->    [];
fetch_regs(N, S) ->    fetch_regs(N-1, S, []).

fetch_regs(0, S, Regs) ->
    [fetch({x,0},S)|Regs];
fetch_regs(I, S, Regs) ->
    fetch_regs(I-1,S,[fetch({x,I},S) | Regs]).

make_blank(Dst, S) ->
    store(Dst, ?BLANK, S).

store({x,I},Val,S=#s{x=X}) ->
    NX = size(X),
    if I >= NX ->
            X1 = list_to_tuple(tuple_to_list(X)++fill_((I+1)-NX,[],Val)),
            S#s { x = X1 };
       true ->
            S#s { x = setelement(I+1, X, Val) }
    end;
store({fr,I},Val,S=#s{f=F}) ->
    NF = size(F),
    if NF =< I ->
            F1 = list_to_tuple(tuple_to_list(F)++fill_((I+1)-NF,0.0,Val)),
            S#s { f = F1 };
       true ->
            S#s { f = setelement(I+1, F, Val) }
    end;
store({y,I}, Val, S=#s{y=Y}) ->
    S#s { y = set_nth_element(I, Y, Val) }.

%% @private
%% set nth element counting from 0!
set_nth_element(0, [_H|T], V) -> [V|T];
set_nth_element(I, [H|T], V) -> [H | set_nth_element(I-1,T,V)].

%% @private
%% generate a lists of N nils (lists:duplicate!)

%%fill(N) ->
%%  fill_(N,[],[]).

fill_(0,_D,_V) -> [];
fill_(1,_D,V) -> [V];
fill_(I,D,V) -> [D | fill_(I-1,D,V)].

%% @private
allocate_(0,_D,Ys) -> Ys;
allocate_(I,D,Ys) -> [D | allocate_(I-1,D,Ys)].

%% @private
deallocate_(0, Ys) -> Ys;
deallocate_(I, [_|Ys]) -> deallocate_(I-1, Ys).

%% @private
fail(S, {f,0}, Class, Reason) ->
    erlang:display({fail,Class,Reason}),
    case S#s.cs of
        [{{f,If},U0}|_] ->
            U1 = length(S#s.y),
            Ys = deallocate_(U1-U0, S#s.y),
            S0 = store({x,0},?BLANK,S),
            S1 = store({x,1},Class,S0),
            S2 = store({x,2},Reason,S1),
            dispatch(S2#s { i=If, y=Ys }); %% was S?
        [] ->
            if Class == thrown ->
                    exit(no_catch);
               Class == exit ->
                    exit(Reason);
               Class == error ->
                    exit({Reason,[stack_trace]})
            end
    end;
fail(S, {f,I1},_Class,_Reason) ->
    dispatch(S#s { i=I1 }).

fail(S, {f,I1}) ->
    dispatch(S#s { i=I1 }).


select_val(Val, [Val,Jump|_]) ->
    Jump;
select_val(Val, [{_,Val},Jump|_]) ->
    Jump;
select_val(Val, [_,_|Select]) ->
    select_val(Val, Select);
select_val(_Val, []) ->
    false.
