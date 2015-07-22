#pragma once
// Generated by codegen/vm_copypaste_impl.py
// You can edit this file, it is not overwritten by generate script

#include "g_process.h"
#include "g_module.h"
#include "bif/g_bif_misc.h"
#include "g_genop.h"
#include "g_predef_atoms.h"

#include <cstring>

namespace gluon {


namespace impl {

struct vm_runtime_ctx_t: runtime_ctx_t {
  // where code begins (for jumps)
  word_t *base;
  Process::stack_t *stack;

  void load(Process *proc) {
    runtime_ctx_t &proc_ctx = proc->get_runtime_ctx();
    mod  = proc_ctx.mod;
    ip   = proc_ctx.ip;
    cp   = proc_ctx.cp;
    live = proc_ctx.live;
    std::memcpy(regs, proc_ctx.regs, sizeof(Term)*live);

    stack = proc->get_stack();
    base  = proc->get_code_base();
    // TODO: update heap top
  }
  void save(Process *proc) {
    runtime_ctx_t &proc_ctx = proc->get_runtime_ctx();
    proc_ctx.mod  = mod;
    proc_ctx.ip   = ip;
    proc_ctx.cp   = cp;
    proc_ctx.live = live;
    std::memcpy(proc_ctx.regs, regs, sizeof(Term)*live);
    // TODO: update heap top
  }
  // TODO: swap_out: save r0, stack top and heap top
  inline void swap_out_light(Process *proc) {
    // update only heap top
  }

  // For special immed1 types (register and stack ref) convert them to their
  // values
  // TODO: move to context in g_vm_impl.h
  void resolve_immed(Term &i) const {
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

  void move(Term val, Term dst) {
#if G_DEBUG
    printf("ctx.move ");
    val.print();
    printf(" -> ");
    dst.println();
#endif
    if (dst.is_regx()) {
      regs[dst.regx_get_value()] = val;
    } else
    if (dst.is_regy()) {
      (*stack)[dst.regy_get_value()] = val;
    } else
#if FEATURE_FLOAT
    if (dst.is_regfp()) {
      regs[dst.regx_get_value()] = val;
    } else
#endif
    {
      G_FAIL("bad move dst")
    }
  }

  void jump(Term t) {
    G_ASSERT(t.is_small());
    printf("ctx.jump -> 0x%zx\n", (word_t)t.small_get_value());
    ip = base + (word_t)t.small_get_value();
    G_ASSERT(ip > base);
    G_ASSERT(ip < mod->m_code.size() + base);
  }

  void alloc_stack(word_t n) {
    stack->reserve(stack->size() + n);
    while (n > 0) {
      (*stack).push_back(Term::make_nil());
      n--;
    }
  }
  void free_stack(word_t n) {
    G_ASSERT(stack->size() >= n);
    stack->resize(stack->size() - n);
  }
  void push(Term t) {
    stack->push_back(t);
  }
  Term pop() {
    G_ASSERT(stack->size() > 0);
    Term t = stack->back();
    stack->pop_back();
    return t;
  }

  // Throws type:reason (for example error:badmatch)
  void raise(Process *proc, Term type, Term reason) {
    swap_out_light(proc);
    regs[0] = type;
    regs[1] = reason;
    proc->m_stack_trace = Term::make_non_value();
    return exception(proc);
  }

  void exception(Process *proc) {
    if (proc->m_catch_level == 0) {
      // we're not catching anything here
      printf("EXCEPTION: ");
      regs[0].print();
      printf(":");
      regs[1].println();
      G_FAIL("Stopping execution here");
    }
    // unwind stack
    for (word_t i = stack->size()-1; i > 0; --i)
    {
      if ((*stack)[i].is_catch()) {
        //word_t jump_to = (*stack)[i].catch_val();
        do {
          i++;
        } while (!(*stack)[i].is_boxed()
                 || !term_tag::is_cp((*stack)[i].boxed_get_ptr<word_t>()));
        // TODO: trim stack to iter


        //ip = catch_jump(index);
        cp = nullptr;
      }

      // TODO: set process exit reason
      proc->get_runtime_ctx().live = 0;
      // TODO: schedule next process in queue
    }
  }
  void push_cp() {
    push(Term::make_boxed_cp(cp));
  }
  void pop_cp() {
    Term p = pop();
    cp = term_tag::untag_cp<word_t>(p.boxed_get_ptr<word_t>());
  }

  void print_args(word_t arity) {
#if G_DEBUG
    for (word_t i = 0; i < arity; ++i) {
      Term value(ip[i]);
      value.print();
      if (value.is_regx() || value.is_regy()) {
        resolve_immed(value);
        printf("=");
        value.print();
      }
      printf(";");
    }
    puts("");
#endif
  }
};

#define DEREF(var) if ((var).is_immed()) { ctx.resolve_immed(var); }

//  inline void opcode_label(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 1
//  }
//  inline void opcode_func_info(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 2
//  }
//  inline void opcode_int_code_end(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 3
//  }
  inline void opcode_call(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 4
    // @spec call Arity Label
    // @doc Call the function at Label.
    //      Save the next instruction as the return address in the CP register.
    Term arity(ctx.ip[0]);
    ctx.live = (word_t)arity.small_get_value();
    ctx.cp = ctx.ip + 2;
    ctx.jump(Term(ctx.ip[1]));
  }
//  inline void opcode_call_last(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 5
//  }
  inline void opcode_call_only(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 6
    // @spec call_only Arity Label
    // @doc Do a tail recursive call to the function at Label.
    //      Do not update the CP register.
    Term arity(ctx.ip[0]);
    ctx.live = (word_t)arity.small_get_value();
    ctx.jump(Term(ctx.ip[1]));
  }
  inline void opcode_call_ext(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 7
    // @spec call_ext Arity Destination
    // @doc Call the function of arity Arity pointed to by Destination.
    //      Save the next instruction as the return address in the CP register.
    // TODO: reduction count
    // TODO: yield and schedule
    // Term arity(ip[0]);
    // TODO: set live on reductions swap out ctx.live = (word_t)arity.small_get_value();
    ctx.cp = ctx.ip + 2;
    //ctx.jump(ctx.ip[2]);
  }
//  inline void opcode_call_ext_last(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 8
//  }
//  inline void opcode_bif0(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 9
//  }
//  inline void opcode_bif1(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 10
//  }
//  inline void opcode_bif2(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 11
//  }
  inline void opcode_allocate(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 12
    // @spec allocate StackNeed Live
    // @doc Allocate space for StackNeed words on the stack. If a GC is needed
    //      during allocation there are Live number of live X registers.
    //      Also save the continuation pointer (CP) on the stack.
    Term stack_need(ctx.ip[1]);
    G_ASSERT(stack_need.is_small());
    ctx.alloc_stack((word_t)stack_need.small_get_value());
    ctx.push_cp();
    ctx.ip += 2;
  }
//  inline void opcode_allocate_heap(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 13
//  }
//  inline void opcode_allocate_zero(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 14
//  }
//  inline void opcode_allocate_heap_zero(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 15
//  }
  inline void opcode_test_heap(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 16
    // @spec test_heap HeapNeed Live
    // @doc Ensure there is space for HeapNeed words on the heap. If a GC is needed
    // save Live number of X registers.
    ctx.ip += 2;
  }
//  inline void opcode_init(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 17
//  }
  inline void opcode_deallocate(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 18
    // @spec deallocate N
    // @doc  Restore the continuation pointer (CP) from the stack and deallocate
    //       N+1 words from the stack (the + 1 is for the CP).
    Term n(ctx.ip[0]);
    ctx.pop_cp();
    ctx.free_stack((word_t)n.small_get_value());
    ctx.ip++;
  }
  inline bool opcode_return(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 19
    // @spec return
    // @doc  Return to the address in the continuation pointer (CP).
    if (!ctx.cp) {
      // nowhere to return: end program
      ctx.save(proc);
      return false; // break vm loop
    }
    ctx.ip = ctx.cp;
    ctx.cp = nullptr;
    return true; // keep running vm loop
  }
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
    // @spec is_lt Lbl Arg1 Arg2
    // @doc Compare two terms and jump to Lbl if Arg1 is not less than Arg2.
    Term    arg1(ctx.ip[1]);
    Term    arg2(ctx.ip[2]);
    DEREF(arg1); // in case they are reg references
    DEREF(arg2);
    if (Term::are_both_small(arg1, arg2)) {
      if (arg1.value() >= arg2.value()) {
        ctx.jump(Term(ctx.ip[0]));
        return;
      }
    } else { // full term compare
      if (!bif::is_term_smaller(arg1, arg2)) {
        ctx.jump(Term(ctx.ip[0]));
        return;
      }
    }
    ctx.ip += 3; // args size
  }
//  inline void opcode_is_ge(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 40
//  }
  inline void opcode_is_eq(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 41
    // @spec is_eq Lbl Arg1 Arg2
    // @doc Compare two terms and jump to Lbl if Arg1 is not (numerically) equal
    // to Arg2.
    Term    arg1(ctx.ip[1]);
    Term    arg2(ctx.ip[2]);
    DEREF(arg1); // in case they are reg references
    DEREF(arg2);
    if (arg1 == arg2 || bif::are_terms_equal(arg1, arg2, false)) {
      ctx.ip += 3;
    } else {
      ctx.jump(Term(ctx.ip[0]));
    }
  }
//  inline void opcode_is_ne(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 42
//  }
  inline void opcode_is_eq_exact(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 43
    // @spec is_eq_exact Lbl Arg1 Arg2
    // @doc Compare two terms and jump to Lbl if Arg1 is not exactly equal to Arg2.
    Term    arg1(ctx.ip[1]);
    Term    arg2(ctx.ip[2]);
    DEREF(arg1); // in case they are reg references
    DEREF(arg2);
    if (arg1 != arg2) {
      // immediate values must be exactly equal or we fail comparison
      if (Term::are_both_immed(arg1, arg2)
          || !bif::are_terms_equal(arg1, arg2, true))
      {
        return ctx.jump(Term(ctx.ip[0]));
      }
    }
    ctx.ip += 3;
  }
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
  inline void opcode_is_nil(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 52
    // @spec is_nil Lbl Arg1
    // @doc Test the type of Arg1 and jump to Lbl if it is not nil.
    Term t(ctx.ip[1]);
    DEREF(t);
    if (t.is_nil() == false) {
      ctx.jump(Term(ctx.ip[0]));
    } else {
      ctx.ip += 2;
    }
  }
//  inline void opcode_is_binary(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 53
//  }
//  inline void opcode_is_constant(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 54
//  }
//  inline void opcode_is_list(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 55
//  }
  inline void opcode_is_nonempty_list(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 56
    // @spec is_nonempty_list Lbl Arg1
    // @doc Test the type of Arg1 and jump to Lbl if it is not a cons.
    Term t(ctx.ip[1]);
    DEREF(t);
    if (t.is_cons() == false) {
      ctx.jump(Term(ctx.ip[0]));
    } else {
      ctx.ip += 2;
    }
  }
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
    Term val(ctx.ip[0]);
    DEREF(val);
    Term dst(ctx.ip[1]);
    ctx.move(val, dst);
    ctx.ip += 2;
  }
  inline void opcode_get_list(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 65
    // @spec get_list  Source Head Tail
    // @doc  Get the head and tail (or car and cdr) parts of a list
    //       (a cons cell) from Source and put them into the registers
    //       Head and Tail.
    Term src(ctx.ip[0]);
    DEREF(src);
    Term dst_head(ctx.ip[1]);
    Term dst_tail(ctx.ip[2]);
    ctx.move(src.cons_head(), dst_head);
    ctx.move(src.cons_tail(), dst_tail);
    ctx.ip += 3;
  }
//  inline void opcode_get_tuple_element(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 66
//  }
//  inline void opcode_set_tuple_element(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 67
//  }
//  inline void opcode_put_string(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 68
//  }
  inline void opcode_put_list(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 69
    // @spec put_list H T Dst
    Term h(ctx.ip[0]);
    Term t(ctx.ip[1]);
    DEREF(h);
    DEREF(t);
    Term dst(ctx.ip[2]);
    Term result = Term::allocate_cons(proc->get_heap(), h, t);
    ctx.move(result, dst);
    ctx.ip += 3;
  }
//  inline void opcode_put_tuple(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 70
//  }
//  inline void opcode_put(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 71
//  }
  inline void opcode_badmatch(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 72
    return ctx.raise(proc, atom::ERROR, atom::BADMATCH);
  }
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
  inline void opcode_gc_bif1(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 124
    // @spec gc_bif1 Lbl Live Bif Arg Reg
    // @doc Call the bif Bif with the argument Arg, and store the result in Reg.
    // On failure jump to Lbl. Do a garbage collection if necessary to allocate
    // space on the heap for the result (saving Live number of X registers).
    Term fun(ctx.ip[0]);
    //Term label(ctx.ip[1]); // on crash?
    //Term live(ctx.ip[2]);
    Term arg1(ctx.ip[3]);
    DEREF(arg1);
    Term result_dst(ctx.ip[4]);

    gc_bif1_fn bif_fn = VM::resolve_bif1(fun);
    G_ASSERT(bif_fn);

    Term result = bif_fn(proc, arg1);
    ctx.move(result, result_dst);
    ctx.ip += 5;
  }
  inline void opcode_gc_bif2(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 125
    // @spec gc_bif2 Lbl Live Bif Arg1 Arg2 Reg
    // @doc Call the bif Bif with the arguments Arg1 and Arg2, and store the
    // result in Reg. On failure jump to Lbl. Do a garbage collection if
    // necessary to allocate space on the heap for the result (saving Live
    // number of X registers).
    Term fun(ctx.ip[0]);
    //Term label(ctx.ip[1]); // on crash?
    //Term live(ctx.ip[2]);
    Term arg1(ctx.ip[3]);
    Term arg2(ctx.ip[4]);
    DEREF(arg1);
    DEREF(arg2);
    Term result_dst(ctx.ip[5]);

    gc_bif2_fn bif_fn = VM::resolve_bif2(fun);
    G_ASSERT(bif_fn);

    Term result = bif_fn(proc, arg1, arg2);
    ctx.move(result, result_dst);
    ctx.ip += 6;
  }
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

