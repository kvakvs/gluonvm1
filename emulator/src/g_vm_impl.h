#pragma once
// Generated by codegen/vm_copypaste_impl.py
// You can edit this file, it is not overwritten by generate script

#include "g_process.h"
#include "g_module.h"
#include "g_code_server.h"
#include "bif/g_bif_misc.h"
#include "g_genop.h"
#include "g_predef_atoms.h"
#include "g_fun.h"
#include "g_heap.h"
#include "g_vm.h"
#include "g_term_helpers.h"

#include <cstring>

namespace gluon {

namespace impl {

struct vm_runtime_ctx_t: runtime_ctx_t {
  // where code begins (for jumps)
  //word_t *base;
  ProcessStack *stack;

  inline void println() {
//#if G_DEBUG
//    puts("---------");
//    printf("CP=");
//    VM::get_cs()->print_mfa(cp);
//    printf("; ");
//    stack->println();
//#endif
  }

  void load(Process *proc) {
    runtime_ctx_t &proc_ctx = proc->get_runtime_ctx();
//    mod  = proc_ctx.mod;
    ip   = proc_ctx.ip;
    cp   = proc_ctx.cp;
    live = proc_ctx.live;
    std::memcpy(regs, proc_ctx.regs, sizeof(Term)*live);

    stack = proc->get_stack();
    //base  = proc->get_code_base();
    // TODO: update heap top
  }
  void save(Process *proc) {
    runtime_ctx_t &proc_ctx = proc->get_runtime_ctx();
//    proc_ctx.mod  = mod;
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
      i = stack->get_y(i.regy_get_value());
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
      word_t x = dst.regx_get_value();
      G_ASSERT(x < sizeof(regs));
      regs[x] = val;
    } else
    if (dst.is_regy()) {
      stack->set_y(dst.regy_get_value(), val);
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

  // Jumps to location pointed with {extfunc, Mod, Fun, Arity} in BEAM ASM,
  // depending on encoding decision: now it would be a {M,F,Arity} tuple in
  // literals table - so fetch MFA elements, resolve address and jump
  void jump_ext(Process *proc, Term mfa_box) {
    G_ASSERT(mfa_box.is_boxed());
    mfarity_t *mfa = mfa_box.boxed_get_ptr<mfarity_t>();
    printf("ctx.jump_ext -> ");
    mfa->println();

    // check for bif, a nonvalue result with error flag set to undef means that
    // this was not a bif
    void *bif_fn = VM::find_bif(*mfa);
    if (bif_fn) {
      Term result = VM::apply_bif(proc, mfa->arity, bif_fn, regs);
      if (result.is_non_value()) {
        if (proc->m_bif_error_reason != atom::UNDEF) {
          // a real error happened
          Term reason = proc->m_bif_error_reason;
          proc->m_bif_error_reason = NONVALUE;
          return raise(proc, atom::ERROR, reason);
        }
        // if it was undef - do nothing, it wasn't a bif - we just continue
      } else {
        // simulate real call but return bif result instead
        regs[0] = result;
        G_ASSERT(cp);
        ip = cp;
        cp = nullptr;
        return;
      }
    }

    auto find_result = VM::get_cs()->find_module(proc, mfa->mod,
                                                 code::LOAD_IF_NOT_FOUND);
    if (find_result.is_error()) {
      G_LOG("ctx.jump_ext: %s\n", find_result.get_error());
      return raise(proc, atom::ERROR, atom::UNDEF);
    }

    Module *mod = find_result.get_result();
    auto find_fn_result = mod->resolve_function(mfa->fun, mfa->arity);
    if (find_fn_result.is_error()) {
      G_LOG("ctx.jump_ext: %s\n", find_fn_result.get_error());
      return raise(proc, atom::ERROR, atom::UNDEF);
    }
    return jump_far(proc, mod, find_fn_result.get_result());
  }

  // Jumps between modules updating base and mod fields
  void jump_far(Process *proc, Module *m, word_t *new_ip) {
//    mod = m;
    //base = proc->get_code_base();
    ip = new_ip;
  }

  void jump(Process *proc, Term t) {
    G_ASSERT(t.is_boxed());
    printf("ctx.jump -> 0x%zx\n", (word_t)t.boxed_get_ptr<word_t>());
    ip = t.boxed_get_ptr<word_t>();
    // TODO: some meaningful assertion here?
    //G_ASSERT(ip > base);
    //G_ASSERT(ip < proc->m_module->m_code.size() + base);
  }

  // Throws type:reason (for example error:badmatch)
  void raise(Process *proc, Term type, Term reason) {
    swap_out_light(proc);
    regs[0] = type;
    regs[1] = reason;
    proc->m_stack_trace = NONVALUE;
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
/*
    for (word_t i = stack->size()-1; i > 0; --i)
    {
      if (stack[i].is_catch()) {
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
      procmo->get_runtime_ctx().live = 0;
      // TODO: schedule next process in queue
    }
*/
  }
  void push_cp() {
    stack->push(Term::make_boxed_cp(cp));
    cp = nullptr;
  }
  void pop_cp() {
    Term p = stack->pop();
    cp = term_tag::untag_cp<word_t>(p.boxed_get_ptr<word_t>());
  }
  inline void stack_allocate(word_t n) {
    stack->push_n_nils(n);
    push_cp();
  }
  inline void stack_deallocate(word_t n) {
    pop_cp();
    stack->drop_n(n);
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
  bool check_bif_error(Process *p) {
    Term reason = p->m_bif_error_reason;
    if (reason.is_non_value()) {
      return false; // good no error
    }
    p->m_bif_error_reason = NONVALUE;
    raise(p, atom::ERROR, reason);
    return true;
  }
};

#define DEREF(var) if ((var).is_immed()) { ctx.resolve_immed(var); }
void opcode_gc_bif1(Process *proc, vm_runtime_ctx_t &ctx);
void opcode_gc_bif2(Process *proc, vm_runtime_ctx_t &ctx);

//  inline void opcode_label(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 1
//  }
  inline void opcode_func_info(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 2
    return ctx.raise(proc, atom::ERROR, atom::FUNCTION_CLAUSE);
  }
//  inline void opcode_int_code_end(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 3
//  }
  inline void opcode_call(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 4
    // @spec call Arity Label
    // @doc Call the function at Label.
    //      Save the next instruction as the return address in the CP register.
    Term arity(ctx.ip[0]);
    ctx.live = arity.small_get_unsigned();
    ctx.cp = ctx.ip + 2;
    ctx.jump(proc, Term(ctx.ip[1]));
  }
  inline void opcode_call_last(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 5
    // @spec call_last Arity Label Deallocate
    // @doc Deallocate and do a tail recursive call to the function at Label.
    // Do not update the CP register. Before the call deallocate Deallocate
    // words of stack.
    Term arity(ctx.ip[0]);
    ctx.live = arity.small_get_unsigned();

    Term n(ctx.ip[2]);
    ctx.stack_deallocate(n.small_get_unsigned());
    ctx.jump(proc, Term(ctx.ip[1]));
  }
  inline void opcode_call_only(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 6
    // @spec call_only Arity Label
    // @doc Do a tail recursive call to the function at Label.
    //      Do not update the CP register.
    Term arity(ctx.ip[0]);
    ctx.live = arity.small_get_unsigned();
    ctx.jump(proc, Term(ctx.ip[1]));
  }
  inline void opcode_call_ext(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 7
    // @spec call_ext Arity Destination
    // @doc Call the function of arity Arity pointed to by Destination.
    //      Save the next instruction as the return address in the CP register.
    // Term arity(ip[0]); // assert arity = destination.arity
    Term boxed_mfa(ctx.ip[1]);
    ctx.cp = ctx.ip + 2;

    Term a(ctx.ip[0]);
    ctx.live = a.small_get_unsigned();
    return ctx.jump_ext(proc, boxed_mfa);
  }
  inline void opcode_call_ext_last(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 8
    // @spec call_ext_last Arity Destination Deallocate
    // @doc Deallocate and do a tail call to function of arity Arity pointed
    // to by Destination. Do not update the CP register. Deallocate Deallocate
    // words from the stack before the call.
    Term boxed_mfa(ctx.ip[1]);
    Term a(ctx.ip[0]);
    Term dealloc(ctx.ip[2]);
    ctx.live = a.small_get_unsigned();
    ctx.stack_deallocate(dealloc.small_get_unsigned());
    return ctx.jump_ext(proc, boxed_mfa);
  }
//  inline void opcode_bif0(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 9
//  }
  inline void opcode_bif1(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 10
    // bif1 Fail import_index Arg1 Dst
    Term boxed_mfa(ctx.ip[1]);
    Term arg1(ctx.ip[2]);
    DEREF(arg1);
    Term result_dst(ctx.ip[3]);

    mfarity_t *mfa = boxed_mfa.boxed_get_ptr<mfarity_t>();
    bif1_fn fn1 = (bif1_fn)VM::find_bif(*mfa);
    G_ASSERT(fn1);

    Term result = fn1(proc, arg1);
    if (ctx.check_bif_error(proc)) { return; }
    ctx.move(result, result_dst);
    ctx.ip += 4;
  }
  inline void opcode_bif2(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 11
    // bif1 Fail import_index Arg1 Arg2 Dst
    Term boxed_mfa(ctx.ip[1]);
    Term arg1(ctx.ip[2]);
    Term arg2(ctx.ip[3]);
    DEREF(arg1);
    DEREF(arg2);
    Term result_dst(ctx.ip[4]);

    mfarity_t *mfa = boxed_mfa.boxed_get_ptr<mfarity_t>();
    bif2_fn fn2 = (bif2_fn)VM::find_bif(*mfa);
    G_ASSERT(fn2);

    Term result = fn2(proc, arg1, arg2);
    if (ctx.check_bif_error(proc)) { return; }
    ctx.move(result, result_dst);
    ctx.ip += 5;
  }
  inline void opcode_allocate(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 12
    // @spec allocate StackNeed Live
    // @doc Allocate space for StackNeed words on the stack. If a GC is needed
    //      during allocation there are Live number of live X registers.
    //      Also save the continuation pointer (CP) on the stack.
    Term stack_need(ctx.ip[0]);
    // TODO: ignore stack contents for speedup (note allocate_zero must fill NILs)
    ctx.stack_allocate(stack_need.small_get_unsigned());
    ctx.ip += 2;
  }
//  inline void opcode_allocate_heap(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 13
//  }
  inline void opcode_allocate_zero(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 14
    // @spec allocate_zero StackNeed Live
    // @doc Allocate space for StackNeed words on the stack. If a GC is needed
    //      during allocation there are Live number of live X registers.
    //      Clear the new stack words. (By writing NIL.)
    //      Also save the continuation pointer (CP) on the stack.
    return opcode_allocate(proc, ctx);
  }
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
    ctx.stack_deallocate(n.small_get_unsigned());
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
      if (arg1.as_word() >= arg2.as_word()) {
        ctx.jump(proc, Term(ctx.ip[0]));
        return;
      }
    } else { // full term compare
      if (!bif::is_term_smaller(arg1, arg2)) {
        ctx.jump(proc, Term(ctx.ip[0]));
        return;
      }
    }
    ctx.ip += 3; // args size
  }
  inline void opcode_is_ge(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 40
    // @spec is_ge Lbl Arg1 Arg2
    // @doc Compare two terms and jump to Lbl if Arg1 is less than Arg2.
    Term    arg1(ctx.ip[1]);
    Term    arg2(ctx.ip[2]);
    DEREF(arg1); // in case they are reg references
    DEREF(arg2);
    if (Term::are_both_small(arg1, arg2)) {
      if (arg1.as_word() < arg2.as_word()) {
        ctx.jump(proc, Term(ctx.ip[0]));
        return;
      }
    } else { // full term compare
      if (bif::is_term_smaller(arg1, arg2)) {
        ctx.jump(proc, Term(ctx.ip[0]));
        return;
      }
    }
    ctx.ip += 3; // args size
  }
  inline void opcode_is_eq(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 41
    // @spec is_eq Lbl Arg1 Arg2
    // @doc Compare two terms and jump to Lbl if Arg1 is not (numerically) equal
    // to Arg2.
    Term    arg1(ctx.ip[1]);
    Term    arg2(ctx.ip[2]);
    DEREF(arg1); // in case they are reg references
    DEREF(arg2);
    if (bif::are_terms_equal(arg1, arg2, false)) {
      ctx.ip += 3;
    } else {
      ctx.jump(proc, Term(ctx.ip[0]));
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
        return ctx.jump(proc, Term(ctx.ip[0]));
      }
    }
    ctx.ip += 3;
  }
//  inline void opcode_is_ne_exact(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 44
//  }
  inline void opcode_is_integer(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 45
    // @spec is_integer Lbl Arg1
    // @doc Test the type of Arg1 and jump to Lbl if it is not an integer.
    Term arg(ctx.ip[1]);
    DEREF(arg);
    if (!arg.is_integer()) {
      return ctx.jump(proc, Term(ctx.ip[0]));
    }
    ctx.ip += 2;
  }
  inline void opcode_is_float(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 46
#if FEATURE_FLOAT
    G_TODO("is_float");
#else
    return ctx.jump(proc, Term(ctx.ip[0]));
#endif
  }
//  inline void opcode_is_number(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 47
//  }
  inline void opcode_is_atom(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 48
    // @spec is_atom Lbl Arg1
    // @doc Test the type of Arg1 and jump to Lbl if it is not an atom.
    Term arg(ctx.ip[1]);
    DEREF(arg);
    if (!arg.is_atom()) {
      return ctx.jump(proc, Term(ctx.ip[0]));
    }
    ctx.ip += 2;
  }
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
      return ctx.jump(proc, Term(ctx.ip[0]));
    }
    ctx.ip += 2;
  }
  inline void opcode_is_binary(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 53
    // @spec is_binary Lbl Arg1
    // @doc Test the type of Arg1 and jump to Lbl if it is not a binary.
#if FEATURE_BINARIES
    G_TODO("is_binary");
#else
    return ctx.jump(proc, Term(ctx.ip[0]));
#endif
  }
//  inline void opcode_is_constant(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 54
//  }
  inline void opcode_is_list(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 55
    // @spec is_list Lbl Arg1
    // @doc Test the type of Arg1 and jump to Lbl if it is not a cons or nil.
    Term t(ctx.ip[1]);
    DEREF(t);
    if (!t.is_cons() && !t.is_nil()) {
      return ctx.jump(proc, Term(ctx.ip[0]));
    }
    ctx.ip += 2;
  }
  inline void opcode_is_nonempty_list(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 56
    // @spec is_nonempty_list Lbl Arg1
    // @doc Test the type of Arg1 and jump to Lbl if it is not a cons.
    Term t(ctx.ip[1]);
    DEREF(t);
    if (t.is_cons() == false) {
      return ctx.jump(proc, Term(ctx.ip[0]));
    }
    ctx.ip += 2;
  }
  inline void opcode_is_tuple(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 57
    // @spec is_tuple Lbl Arg1
    // @doc Test the type of Arg1 and jump to Lbl if it is not a tuple.
    Term t(ctx.ip[1]);
    DEREF(t);
    if (!t.is_tuple()) {
      return ctx.jump(proc, Term(ctx.ip[0]));
    }
    ctx.ip += 2;
  }
  inline void opcode_test_arity(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 58
    // @spec test_arity Lbl Arg1 Arity
    // @doc Test the arity of (the tuple in) Arg1 and jump to Lbl if it is
    // not equal to Arity.
    Term t(ctx.ip[1]);
    DEREF(t);
    Term t_arity(ctx.ip[2]);
    DEREF(t_arity);
    if (!t.is_tuple() || t.tuple_get_arity() != t_arity.small_get_unsigned()) {
      return ctx.jump(proc, Term(ctx.ip[0]));
    }
    ctx.ip += 3;
  }
  inline void opcode_select_val(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 59
    // @spec select_val Arg FailLabel Destinations
    // @doc Jump to the destination label corresponding to Arg
    //      in the Destinations list, if no arity matches, jump to FailLabel.
    Term arg(ctx.ip[0]);
    DEREF(arg);

    Term dst(ctx.ip[2]);
    // TODO: binary search
    Term *elements = dst.boxed_get_ptr<Term>();
    word_t dst_arity = ((word_t *)elements)[0] / 2;
    for (word_t i = 0; i < dst_arity; i++) {
      if (elements[i*2+1] == arg) {
        return ctx.jump(proc, elements[i*2+2]);
      }
    }
    ctx.jump(proc, Term(ctx.ip[1]));
  }
//  inline void opcode_select_tuple_arity(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 60
//  }
  inline void opcode_jump(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 61
    return ctx.jump(proc, Term(ctx.ip[0]));
  }
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
  inline void opcode_get_tuple_element(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 66
    // @spec get_tuple_element Source Element Destination
    // @doc  Get element number Element from the tuple in Source and put
    //       it in the destination register Destination.
    Term src(ctx.ip[0]);
    DEREF(src);
    Term t_el(ctx.ip[1]);
    DEREF(t_el);
    word_t el = t_el.small_get_unsigned();
    G_ASSERT(el >= 0 && el < src.tuple_get_arity());
    ctx.move(src.tuple_get_element(el), Term(ctx.ip[2]));
    ctx.ip += 3;
  }
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
  inline void opcode_put_tuple(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 70
    // @spec put_tuple Arity Dst
    // followed by Arity 'put N' commands
    Term t_arity(ctx.ip[0]);
    Term dst(ctx.ip[1]);
    ctx.ip += 2;

    word_t arity = t_arity.small_get_unsigned();
    if (G_UNLIKELY(arity == 0)) {
      ctx.move(Term::make_zero_tuple(), dst);
      return;
    }

    Term *elements = (Term *)proc->heap_alloc(arity+1);
    Term *p = elements+1;
    do {
      // assert that ip[0] is opcode 'put' skip it and read its argument
      Term value(ctx.ip[1]);
      ctx.ip += 2;
      DEREF(value);
        printf("put ");
        value.println();
      *p = value;
      p++;
    } while (--arity != 0);

    ctx.move(Term::make_tuple(elements, t_arity.small_get_unsigned()), dst);
  }
//  inline void opcode_put(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 71
//  }
  inline void opcode_badmatch(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 72
    return ctx.raise(proc, atom::ERROR, atom::BADMATCH);
  }
  inline void opcode_if_end(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 73
    return ctx.raise(proc, atom::ERROR, atom::IF_CLAUSE);
  }
  inline void opcode_case_end(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 74
    return ctx.raise(proc, atom::ERROR, atom::CASE_CLAUSE);
  }
  inline void opcode_call_fun(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 75
    // @spec call_fun Arity
    // @doc Call a fun of arity Arity. Assume arguments in registers x(0) to
    // x(Arity-1) and that the fun is in x(Arity). Save the next instruction as
    // the return address in the CP register.
    Term t_arity(ctx.ip[0]);
    G_ASSERT(t_arity.is_small());

    word_t arity = t_arity.small_get_unsigned();

    Term fun(ctx.regs[arity]);
    if (fun.is_boxed_fun()) {
      boxed_fun_t *bf = fun.boxed_get_ptr<boxed_fun_t>();
      if (bf->get_arity() != arity + bf->get_num_free()) {
        // TODO: make tuple {badarity, f_args}
        return ctx.raise(proc, atom::ERROR, atom::BADARITY);
      }
      // TODO: if bf.fe is null - unloaded fun
      word_t num_free = bf->get_num_free();
      G_ASSERT(arity + num_free < VM_MAX_REGS);
      std::copy(bf->frozen, bf->frozen + num_free, ctx.regs + arity);

      ctx.live = arity;
      ctx.cp   = ctx.ip + 1;
      ctx.ip   = bf->fe->code;
      G_ASSERT(ctx.ip);
      return;
    } else if (fun.is_boxed_export()) {
      export_t *ex = fun.boxed_get_ptr<export_t>();
      G_ASSERT(arity == ex->mfa.arity);
      if (ex->is_bif()) {
        Term result = VM::apply_bif(proc, arity, ex->code, ctx.regs);
        if (ctx.check_bif_error(proc)) { return; }
        ctx.regs[0] = result;
        ctx.ip++;
      } else {
        ctx.live = arity;
        ctx.cp   = ctx.ip + 1;
        ctx.ip   = ex->code;
        return;
      }
    } else {
      // TODO: make tuple {badfun, f_args} (same as above)
      term::TupleBuilder tb(proc->get_heap(), 2);
      tb.add(atom::BADFUN);
      tb.add(fun);
      return ctx.raise(proc, atom::ERROR, tb.make_tuple());
    }
  }
//  inline void opcode_make_fun(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 76
//  }
//  inline void opcode_is_function(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 77
//  }
  inline void opcode_call_ext_only(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 78
    // @spec call_ext_only Arity Label
    // Do a tail recursive call to the function at Label. Do not update CP.
    Term boxed_mfa(ctx.ip[1]);
    Term a(ctx.ip[0]);
    ctx.live = a.small_get_unsigned();
    return ctx.jump_ext(proc, boxed_mfa);
  }
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
  inline void opcode_make_fun2(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 103
    // @spec make_fun2 LambdaT_index
    // @doc Produces a callable fun object
    Term boxed_fe(ctx.ip[0]);
    fun_entry_t *fe = boxed_fe.boxed_get_ptr<fun_entry_t>();

    // TODO: mark boxed memory somehow so GC can recognize its size?
    word_t *p8 = proc->heap_alloc(
          ProcessHeap::calculate_word_size(sizeof(boxed_fun_t)) + fe->num_free
          );
    // Assuming that regs have all values ready to be frozen (copied to closure)
    boxed_fun_t *p = fun::box_fun(fe, p8, proc->get_pid(), ctx.regs);

    ctx.regs[0] = FunObject::make(p);
    ctx.ip += 1;
  }
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
  inline void opcode_is_function2(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 115
    // @spec is_function2 Lbl Arg1 Arity
    // @doc Test the type of Arg1 and jump to Lbl if it is not a function
    // of arity Arity.
    Term arg1(ctx.ip[1]);
    DEREF(arg1);
    Term arity(ctx.ip[2]);
    DEREF(arity);
    // Check if its fun at all
    if (arg1.is_boxed_fun()) {
      // check arity
      boxed_fun_t *bf = arg1.boxed_get_ptr<boxed_fun_t>();
      if (arity.small_get_unsigned() + bf->get_num_free() != bf->get_arity()) {
        return ctx.jump(proc, Term(ctx.ip[0]));
      }
    } else if (arg1.is_boxed_export()) {
      // check arity
      export_t *ex = arg1.boxed_get_ptr<export_t>();
      if (arity.small_get_unsigned() != ex->mfa.arity) {
        return ctx.jump(proc, Term(ctx.ip[0]));
      }
    } else {
      return ctx.jump(proc, Term(ctx.ip[0]));
    }

    ctx.ip += 3;
  }
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
    Term boxed_fun(ctx.ip[2]);
    Term arg1(ctx.ip[3]);
    DEREF(arg1);
    Term result_dst(ctx.ip[4]);

    mfarity_t *mfa = boxed_fun.boxed_get_ptr<mfarity_t>();
    bif1_fn fn1 = (bif1_fn)VM::find_bif(*mfa);
    G_ASSERT(fn1);

    Term result = fn1(proc, arg1);
    if (ctx.check_bif_error(proc)) { return; }
    ctx.move(result, result_dst);
    ctx.ip += 5;
  }
  inline void opcode_gc_bif2(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 125
    // @spec gc_bif2 Lbl Live Bif Arg1 Arg2 Reg
    // @doc Call the bif Bif with the arguments Arg1 and Arg2, and store the
    // result in Reg. On failure jump to Lbl. Do a garbage collection if
    // necessary to allocate space on the heap for the result (saving Live
    // number of X registers).
    Term boxed_fun(ctx.ip[2]);
    Term arg1(ctx.ip[3]);
    Term arg2(ctx.ip[4]);
    DEREF(arg1);
    DEREF(arg2);
    Term result_dst(ctx.ip[5]);

    mfarity_t *mfa = boxed_fun.boxed_get_ptr<mfarity_t>();
    bif2_fn fn2 = (bif2_fn)VM::find_bif(*mfa);
    G_ASSERT(fn2);

    Term result = fn2(proc, arg1, arg2);
    if (ctx.check_bif_error(proc)) { return; }
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
  inline void opcode_trim(Process *proc, vm_runtime_ctx_t &ctx) { // opcode: 136
    // @spec trim N Remaining
    // @doc Reduce the stack usage by N words, keeping the CP on the top.
    Term n(ctx.ip[0]);
    auto masq_cp = ctx.stack->pop();
    ctx.stack->drop_n(n.small_get_unsigned());
    ctx.stack->push(masq_cp);
    ctx.ip += 2;
  }
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

