#pragma once
// Generated by codegen/vm_copypaste_impl.py

#include "g_process.h"
#include "bif/g_bif_misc.h"
#include <cstring>

namespace gluon {


namespace impl {

struct vm_runtime_ctx_t: runtime_ctx_t {
  // where code begins (for jumps)
  word_t *base;
  Process::stack_t *stack;

  inline void load(Process *proc) {
    const runtime_ctx_t &proc_ctx = proc->get_runtime_ctx();
    mod  = proc_ctx.mod;
    ip   = proc_ctx.ip;
    std::memcpy(regs, proc_ctx.regs, sizeof(regs)); // TODO: more efficient copy?

    stack = proc->get_stack();
    base  = proc->get_code_base();
  }

  // For special immed1 types (register and stack ref) convert them to their
  // values
  // TODO: move to context in g_vm_impl.h
  inline void resolve_immed(Term &i) const {
    if (i.is_regx()) {
      i = regs[i.regx_get_value()];
    }
    else if (i.is_regy()) {
      i = (*stack)[i.regy_get_value()];
    }
#if FEATURE_FLOAT
    else if (i.is_regfp()) {
      i = fp_regs[i.regfp_get_value()];
    }
#endif
  }

};

#define IMMED(var) if ((var).is_immed()) { ctx.resolve_immed(var); }

//  inline void opcode_label(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 1
//  }
//  inline void opcode_func_info(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 2
//  }
//  inline void opcode_int_code_end(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 3
//  }
//  inline void opcode_call(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 4
//  }
//  inline void opcode_call_last(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 5
//  }
  inline void opcode_call_only(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 6
    //Term arg1 = ctx.ip[0];
    Term arg2 = ctx.ip[1];

    G_ASSERT(arg2.is_small());
    word_t offset = (word_t)arg2.small_get_value();
    ctx.ip = proc->get_code_base() + offset;
  }
//  inline void opcode_call_ext(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 7
//  }
//  inline void opcode_call_ext_last(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 8
//  }
//  inline void opcode_bif0(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 9
//  }
//  inline void opcode_bif1(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 10
//  }
//  inline void opcode_bif2(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 11
//  }
//  inline void opcode_allocate(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 12
//  }
//  inline void opcode_allocate_heap(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 13
//  }
//  inline void opcode_allocate_zero(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 14
//  }
//  inline void opcode_allocate_heap_zero(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 15
//  }
//  inline void opcode_test_heap(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 16
//  }
//  inline void opcode_init(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 17
//  }
//  inline void opcode_deallocate(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 18
//  }
//  inline void opcode_return(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 19
//  }
//  inline void opcode_send(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 20
//  }
//  inline void opcode_remove_message(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 21
//  }
//  inline void opcode_timeout(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 22
//  }
//  inline void opcode_loop_rec(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 23
//  }
//  inline void opcode_loop_rec_end(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 24
//  }
//  inline void opcode_wait(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 25
//  }
//  inline void opcode_wait_timeout(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 26
//  }
//  inline void opcode_m_plus(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 27
//  }
//  inline void opcode_m_minus(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 28
//  }
//  inline void opcode_m_times(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 29
//  }
//  inline void opcode_m_div(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 30
//  }
//  inline void opcode_int_div(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 31
//  }
//  inline void opcode_int_rem(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 32
//  }
//  inline void opcode_int_band(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 33
//  }
//  inline void opcode_int_bor(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 34
//  }
//  inline void opcode_int_bxor(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 35
//  }
//  inline void opcode_int_bsl(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 36
//  }
//  inline void opcode_int_bsr(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 37
//  }
//  inline void opcode_int_bnot(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 38
//  }
  inline void opcode_is_lt(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 39
    Term on_fail = ctx.ip[0];
    Term arg1    = ctx.ip[1];
    Term arg2    = ctx.ip[2];
    IMMED(arg1); // in case they are reg references
    IMMED(arg2);
    if (Term::are_both_small(arg1, arg2)) {
      if (arg1.value() >= arg2.value()) {
        ctx.ip = ctx.base + (word_t)on_fail.small_get_value();
        return;
      }
    } else { // full term compare
      if (!bif::is_term_smaller(arg1, arg2)) {
        ctx.ip = ctx.base + (word_t)on_fail.small_get_value();
        return;
      }
    }
    ctx.ip += 3; // args size
  }
//  inline void opcode_is_ge(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 40
//  }
//  inline void opcode_is_eq(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 41
//  }
//  inline void opcode_is_ne(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 42
//  }
//  inline void opcode_is_eq_exact(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 43
//  }
//  inline void opcode_is_ne_exact(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 44
//  }
//  inline void opcode_is_integer(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 45
//  }
//  inline void opcode_is_float(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 46
//  }
//  inline void opcode_is_number(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 47
//  }
//  inline void opcode_is_atom(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 48
//  }
//  inline void opcode_is_pid(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 49
//  }
//  inline void opcode_is_reference(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 50
//  }
//  inline void opcode_is_port(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 51
//  }
//  inline void opcode_is_nil(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 52
//  }
//  inline void opcode_is_binary(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 53
//  }
//  inline void opcode_is_constant(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 54
//  }
//  inline void opcode_is_list(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 55
//  }
//  inline void opcode_is_nonempty_list(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 56
//  }
//  inline void opcode_is_tuple(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 57
//  }
//  inline void opcode_test_arity(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 58
//  }
//  inline void opcode_select_val(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 59
//  }
//  inline void opcode_select_tuple_arity(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 60
//  }
//  inline void opcode_jump(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 61
//  }
//  inline void opcode_catch(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 62
//  }
//  inline void opcode_catch_end(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 63
//  }

inline void opcode_move(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 64
  Term arg1 = Term(ctx.ip[0]);
  Term arg2 = Term(ctx.ip[1]);
  if (arg2.is_regx()) {
    ctx.regs[arg2.regx_get_value()] = arg1;
  } else {
    G_FAIL("bad move dst")
  }
  ctx.ip += 2;
}
//  inline void opcode_get_list(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 65
//  }
//  inline void opcode_get_tuple_element(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 66
//  }
//  inline void opcode_set_tuple_element(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 67
//  }
//  inline void opcode_put_string(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 68
//  }
//  inline void opcode_put_list(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 69
//  }
//  inline void opcode_put_tuple(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 70
//  }
//  inline void opcode_put(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 71
//  }
//  inline void opcode_badmatch(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 72
//  }
//  inline void opcode_if_end(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 73
//  }
//  inline void opcode_case_end(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 74
//  }
//  inline void opcode_call_fun(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 75
//  }
//  inline void opcode_make_fun(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 76
//  }
//  inline void opcode_is_function(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 77
//  }
//  inline void opcode_call_ext_only(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 78
//  }
//  inline void opcode_bs_start_match(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 79
//  }
//  inline void opcode_bs_get_integer(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 80
//  }
//  inline void opcode_bs_get_float(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 81
//  }
//  inline void opcode_bs_get_binary(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 82
//  }
//  inline void opcode_bs_skip_bits(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 83
//  }
//  inline void opcode_bs_test_tail(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 84
//  }
//  inline void opcode_bs_save(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 85
//  }
//  inline void opcode_bs_restore(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 86
//  }
//  inline void opcode_bs_init(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 87
//  }
//  inline void opcode_bs_final(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 88
//  }
//  inline void opcode_bs_put_integer(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 89
//  }
//  inline void opcode_bs_put_binary(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 90
//  }
//  inline void opcode_bs_put_float(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 91
//  }
//  inline void opcode_bs_put_string(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 92
//  }
//  inline void opcode_bs_need_buf(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 93
//  }
//  inline void opcode_fclearerror(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 94
//  }
//  inline void opcode_fcheckerror(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 95
//  }
//  inline void opcode_fmove(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 96
//  }
//  inline void opcode_fconv(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 97
//  }
//  inline void opcode_fadd(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 98
//  }
//  inline void opcode_fsub(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 99
//  }
//  inline void opcode_fmul(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 100
//  }
//  inline void opcode_fdiv(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 101
//  }
//  inline void opcode_fnegate(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 102
//  }
//  inline void opcode_make_fun2(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 103
//  }
//  inline void opcode_try(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 104
//  }
//  inline void opcode_try_end(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 105
//  }
//  inline void opcode_try_case(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 106
//  }
//  inline void opcode_try_case_end(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 107
//  }
//  inline void opcode_raise(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 108
//  }
//  inline void opcode_bs_init2(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 109
//  }
//  inline void opcode_bs_bits_to_bytes(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 110
//  }
//  inline void opcode_bs_add(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 111
//  }
//  inline void opcode_apply(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 112
//  }
//  inline void opcode_apply_last(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 113
//  }
//  inline void opcode_is_boolean(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 114
//  }
//  inline void opcode_is_function2(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 115
//  }
//  inline void opcode_bs_start_match2(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 116
//  }
//  inline void opcode_bs_get_integer2(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 117
//  }
//  inline void opcode_bs_get_float2(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 118
//  }
//  inline void opcode_bs_get_binary2(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 119
//  }
//  inline void opcode_bs_skip_bits2(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 120
//  }
//  inline void opcode_bs_test_tail2(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 121
//  }
//  inline void opcode_bs_save2(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 122
//  }
//  inline void opcode_bs_restore2(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 123
//  }
//  inline void opcode_gc_bif1(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 124
//  }
//  inline void opcode_gc_bif2(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 125
//  }
//  inline void opcode_bs_final2(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 126
//  }
//  inline void opcode_bs_bits_to_bytes2(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 127
//  }
//  inline void opcode_put_literal(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 128
//  }
//  inline void opcode_is_bitstr(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 129
//  }
//  inline void opcode_bs_context_to_binary(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 130
//  }
//  inline void opcode_bs_test_unit(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 131
//  }
//  inline void opcode_bs_match_string(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 132
//  }
//  inline void opcode_bs_init_writable(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 133
//  }
//  inline void opcode_bs_append(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 134
//  }
//  inline void opcode_bs_private_append(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 135
//  }
//  inline void opcode_trim(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 136
//  }
//  inline void opcode_bs_init_bits(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 137
//  }
//  inline void opcode_bs_get_utf8(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 138
//  }
//  inline void opcode_bs_skip_utf8(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 139
//  }
//  inline void opcode_bs_get_utf16(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 140
//  }
//  inline void opcode_bs_skip_utf16(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 141
//  }
//  inline void opcode_bs_get_utf32(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 142
//  }
//  inline void opcode_bs_skip_utf32(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 143
//  }
//  inline void opcode_bs_utf8_size(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 144
//  }
//  inline void opcode_bs_put_utf8(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 145
//  }
//  inline void opcode_bs_utf16_size(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 146
//  }
//  inline void opcode_bs_put_utf16(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 147
//  }
//  inline void opcode_bs_put_utf32(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 148
//  }
//  inline void opcode_on_load(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 149
//  }
//  inline void opcode_recv_mark(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 150
//  }
//  inline void opcode_recv_set(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 151
//  }
//  inline void opcode_gc_bif3(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 152
//  }
//  inline void opcode_line(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 153
//  }
//  inline void opcode_put_map_assoc(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 154
//  }
//  inline void opcode_put_map_exact(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 155
//  }
//  inline void opcode_is_map(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 156
//  }
//  inline void opcode_has_map_fields(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 157
//  }
//  inline void opcode_get_map_elements(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 158
//  }

} // ns impl
} // ns gluon

