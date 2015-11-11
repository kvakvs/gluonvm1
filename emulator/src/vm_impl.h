#pragma once

#include "process.h"
#include "vm_ctx.h"
#include "functional.h"

namespace gluon {

namespace impl {

//  inline void opcode_label(Process *proc, VMRuntimeContext &ctx) { // opcode:
//  1
//  }
inline void opcode_func_info(Process* proc,
                             VMRuntimeContext& ctx) {  // opcode: 2
  return ctx.raise(proc, atom::ERROR, atom::FUNCTION_CLAUSE);
}
//  inline void opcode_int_code_end(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 3
//  }
inline WantSchedule opcode_call(Process* proc,
                                VMRuntimeContext& ctx) {  // opcode: 4
  // @spec call Arity Label
  // @doc Call the function at Label.
  //      Save the next instruction as the return address in the CP register.
  Term arity(ctx.ip(0));
  ctx.live = arity.small_word();
  ctx.set_cp(ctx.ip() + 2);
  ctx.jump(proc, ContinuationPointer(ctx.ip(1)));
  return ctx.consume_reduction(proc);
}
inline WantSchedule opcode_call_last(Process* proc,
                                     VMRuntimeContext& ctx) {  // opcode: 5
  // @spec call_last Arity Label Deallocate
  // @doc Deallocate and do a tail recursive call to the function at Label.
  // Do not update the CP register. Before the call deallocate Deallocate
  // words of stack.
  Term arity(ctx.ip(0));
  ctx.live = arity.small_word();

  Term n(ctx.ip(2));
  ctx.stack_deallocate(n.small_word());
  ctx.jump(proc, ContinuationPointer(ctx.ip(1)));
  return ctx.consume_reduction(proc);
}
inline WantSchedule opcode_call_only(Process* proc,
                                     VMRuntimeContext& ctx) {  // opcode: 6
  // @spec call_only Arity Label
  // @doc Do a tail recursive call to the function at Label.
  //      Do not update the CP register.
  Term arity(ctx.ip(0));
  ctx.live = arity.small_word();
  ctx.jump(proc, ContinuationPointer(ctx.ip(1)));
  return ctx.consume_reduction(proc);
}
inline WantSchedule opcode_call_ext(Process* proc,
                                    VMRuntimeContext& ctx) {  // opcode: 7
  // @spec call_ext Arity Destination
  // @doc Call the function of arity Arity pointed to by Destination.
  //      Save the next instruction as the return address in the CP register.
  // Term arity(ip[0]); // assert arity = destination.arity
  Term boxed_mfa(ctx.ip(1));
  ctx.set_cp(ctx.ip() + 2);

  Term a(ctx.ip(0));
  ctx.live = a.small_word();
  ctx.jump_ext(proc, boxed_mfa);
  return ctx.consume_reduction(proc);
}
inline WantSchedule opcode_call_ext_last(Process* proc,
                                         VMRuntimeContext& ctx) {  // opcode: 8
  // @spec call_ext_last Arity Destination Deallocate
  // @doc Deallocate and do a tail call to function of arity Arity pointed
  // to by Destination. Do not update the CP register. Deallocate Deallocate
  // words from the stack before the call.
  Term a(ctx.ip(0));
  Term boxed_mfa(ctx.ip(1));
  Term dealloc(ctx.ip(2));
  ctx.live = a.small_word();
  ctx.stack_deallocate(dealloc.small_word());
  ctx.jump_ext(proc, boxed_mfa);
  return ctx.consume_reduction(proc);
}

inline WantSchedule opcode_bif0(Process* proc,
                                VMRuntimeContext& ctx) {  // opcode: 9
  // bif0 import_index Dst (note no fail label)
  return opcode_bif<0>(proc, ctx);
}

inline WantSchedule opcode_bif1(Process* proc,
                                VMRuntimeContext& ctx) {  // opcode: 10
  // bif1 Fail import_index Arg1 Arg2 Dst
  return opcode_bif<1>(proc, ctx);
}

inline WantSchedule opcode_bif2(Process* proc,
                                VMRuntimeContext& ctx) {  // opcode: 11
  // bif2 Fail import_index Arg1 Arg2 Dst
  return opcode_bif<2>(proc, ctx);
}

inline void opcode_allocate(Process* proc,
                            VMRuntimeContext& ctx) {  // opcode: 12
  // @spec allocate StackNeed Live
  // @doc Allocate space for StackNeed words on the stack. If a GC is needed
  //      during allocation there are Live number of live X registers.
  //      Also save the continuation pointer (CP) on the stack.
  Term stack_need(ctx.ip(0));
  // TODO: ignore stack contents for speedup (note allocate_zero must fill NILs)
  ctx.stack_allocate(stack_need.small_word());
  ctx.step_ip(2);
}

inline void opcode_allocate_heap(Process* proc,
                                 VMRuntimeContext& ctx) {  // opcode: 13
  // @spec allocate_heap StackNeed HeapNeed Live
  // @doc Allocate space for StackNeed words on the stack and ensure there is
  //      space for HeapNeed words on the heap. If a GC is needed
  //      save Live number of X registers.
  //      Also save the continuation pointer (CP) on the stack.
  Term stack_need(ctx.ip(0));
  ctx.stack_allocate(stack_need.small_word());
  ctx.step_ip(3);
}

inline void opcode_allocate_zero(Process* proc,
                                 VMRuntimeContext& ctx) {  // opcode: 14
  // @spec allocate_zero StackNeed Live
  // @doc Allocate space for StackNeed words on the stack. If a GC is needed
  //      during allocation there are Live number of live X registers.
  //      Clear the new stack words. (By writing NIL.)
  //      Also save the continuation pointer (CP) on the stack.
  return opcode_allocate(proc, ctx);
}

inline void opcode_allocate_heap_zero(Process* proc,
                                      VMRuntimeContext& ctx) {  // opcode: 15
  // @spec allocate_heap_zero StackNeed HeapNeed Live
  // @doc Allocate space for StackNeed words on the stack and HeapNeed words
  // on the heap. If a GC is needed during allocation there are Live number
  // of live X registers. Clear the new stack words. (By writing NIL.) Also
  // save the continuation pointer (CP) on the stack.
  Term stack_need(ctx.ip(0));
  ctx.stack_allocate(stack_need.small_word());
  ctx.step_ip(3);
}

inline void opcode_test_heap(Process* proc,
                             VMRuntimeContext& ctx) {  // opcode: 16
  // @spec test_heap HeapNeed Live
  // @doc Ensure there is space for HeapNeed words on the heap. If a GC is
  // needed
  // save Live number of X registers.
  ctx.step_ip(2);
}
inline void opcode_init(Process* proc, VMRuntimeContext& ctx) {  // opcode: 17
  // @spec init N
  // @doc  Clear the Nth stack word. (By writing NIL.)
  Term y_reg(ctx.ip(0));
  G_ASSERT(y_reg.is_regy());
  ctx.stack().set_y(y_reg.regy_get_value(), term::nil_as_word);
  ctx.inc_ip();
}
inline void opcode_deallocate(Process* proc,
                              VMRuntimeContext& ctx) {  // opcode: 18
  // @spec deallocate N
  // @doc  Restore the continuation pointer (CP) from the stack and deallocate
  //       N+1 words from the stack (the + 1 is for the CP).
  Term n(ctx.ip(0));
  ctx.stack_deallocate(n.small_word());
  ctx.inc_ip();
}
inline WantSchedule opcode_return(Process* proc,
                                  VMRuntimeContext& ctx) {  // opcode: 19
  // @spec return
  // @doc  Return to the address in the continuation pointer (CP).
  if (!ctx.cp()) {
    // nowhere to return: end process
    proc->finished();
    ctx.swap_out(proc);
    return WantSchedule::NextProcess;
  }
  ctx.set_ip(ctx.cp());
  ctx.set_cp(nullptr);
  return WantSchedule::KeepGoing;
}

inline WantSchedule opcode_send(Process* proc,
                                VMRuntimeContext& ctx) {  // opcode: 20
  // @spec send
  // @doc  Send argument in x(1) as a message to the destination process in
  // x(0).
  //       The message in x(1) ends up as the result of the send in x(0).
  Term dest(ctx.regs_[0]);
  Term msg(ctx.regs_[1]);

  Std::fmt("send ");
  msg.print(ctx.vm_);
  Std::fmt(" -> ");
  dest.println(ctx.vm_);

  proc->msg_send(dest, msg);
  ctx.regs_[0] = ctx.regs_[1];
  ctx.regs_[1] = the_nil;
  return ctx.consume_reduction(proc);
}

inline void opcode_remove_message(Process* proc,
                                  VMRuntimeContext& ctx) {  // opcode: 21
  // @spec remove_message
  // @doc  Unlink the current message from the message queue and store a
  //       pointer to the message in x(0). Remove any timeout.
  ctx.regs_[0] = proc->mailbox().get_current();
  proc->mailbox().remove_current();
}

//  inline void opcode_timeout(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 22
//  }

inline void opcode_loop_rec(Process* proc,
                            VMRuntimeContext& ctx) {  // opcode: 23
  // @spec loop_rec Label Source
  // @doc  Pick up the next message and place it in x(0). If no message,
  //       jump to a wait or wait_timeout instruction.
  Term msg = proc->mailbox().get_current();
  if (msg.is_non_value()) {
    return ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
  }
  // ctx.move(msg, Term(ctx.ip[1]));
  Std::fmt("loop_rec msg=");
  msg.println(ctx.vm_);

  ctx.regs_[0] = msg;
  ctx.step_ip(2);
}

inline void opcode_loop_rec_end(Process* proc,
                                VMRuntimeContext& ctx) {  // opcode: 24
  // @spec loop_rec_end Label
  // @doc  Advance the save pointer to the next message (the current
  // message didn't match), then jump to the Label (loop_rec instruction).
  proc->mailbox().step_next();
  ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
}

inline void opcode_wait(Process* p, VMRuntimeContext& ctx) {  // opcode: 25
  // @spec wait Label
  // @doc  Suspend the processes and set the entry point to the beginning of the
  //       receive loop at Label.

  // Schedule out
  p->set_slice_result(proc::SliceResult::Wait);
  ctx.jump(p, ContinuationPointer(ctx.ip(0)));
  // ctx.swap_out_partial(p); - done by VM loop
  // we always yield after wait
}

//  inline void opcode_wait_timeout(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 26
//  }
//  inline void opcode_m_plus(Process *proc, VMRuntimeContext &ctx) { // opcode:
//  27
//  }
//  inline void opcode_m_minus(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 28
//  }
//  inline void opcode_m_times(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 29
//  }
//  inline void opcode_m_div(Process *proc, VMRuntimeContext &ctx) { // opcode:
//  30
//  }
//  inline void opcode_int_div(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 31
//  }
//  inline void opcode_int_rem(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 32
//  }
//  inline void opcode_int_band(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 33
//  }
//  inline void opcode_int_bor(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 34
//  }
//  inline void opcode_int_bxor(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 35
//  }
//  inline void opcode_int_bsl(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 36
//  }
//  inline void opcode_int_bsr(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 37
//  }
//  inline void opcode_int_bnot(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 38
//  }
inline void opcode_is_lt(Process* proc, VMRuntimeContext& ctx) {  // opcode: 39
  // @spec is_lt Lbl Arg1 Arg2
  // @doc Compare two terms and jump to Lbl if Arg1 is not less than Arg2.
  Term arg1(ctx.ip(1));
  Term arg2(ctx.ip(2));
  ctx.deref(arg1);  // in case they are reg references
  ctx.deref(arg2);
  if (Term::are_both_small(arg1, arg2)) {
    if (arg1.value() >= arg2.value()) {
      ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
      return;
    }
  } else {  // full term compare
    if (!bif::is_term_smaller(ctx.vm_, arg1, arg2)) {
      ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
      return;
    }
  }
  ctx.step_ip(3);  // args size
}
inline void opcode_is_ge(Process* proc, VMRuntimeContext& ctx) {  // opcode: 40
  // @spec is_ge Lbl Arg1 Arg2
  // @doc Compare two terms and jump to Lbl if Arg1 is less than Arg2.
  Term arg1(ctx.ip(1));
  Term arg2(ctx.ip(2));
  ctx.deref(arg1);  // in case they are reg references
  ctx.deref(arg2);
  if (Term::are_both_small(arg1, arg2)) {
    if (arg1.value() < arg2.value()) {
      ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
      return;
    }
  } else {  // full term compare
    if (bif::is_term_smaller(ctx.vm_, arg1, arg2)) {
      ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
      return;
    }
  }
  ctx.step_ip(3);  // args size
}
inline void opcode_is_eq(Process* proc, VMRuntimeContext& ctx) {  // opcode: 41
  // @spec is_eq Lbl Arg1 Arg2
  // @doc Compare two terms and jump to Lbl if Arg1 is not (numerically) equal
  // to Arg2.
  Term arg1(ctx.ip(1));
  Term arg2(ctx.ip(2));
  ctx.deref(arg1);  // in case they are reg references
  ctx.deref(arg2);
  if (bif::are_terms_equal(ctx.vm_, arg1, arg2, false)) {
    ctx.step_ip(3);
  } else {
    ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
  }
}
//  inline void opcode_is_ne(Process *proc, VMRuntimeContext &ctx) { // opcode:
//  42
//  }
inline void opcode_is_eq_exact(Process* proc,
                               VMRuntimeContext& ctx) {  // opcode: 43
  // @spec is_eq_exact Lbl Arg1 Arg2
  // @doc Compare two terms and jump to Lbl if Arg1 is not exactly equal to
  // Arg2.
  Term arg1(ctx.ip(1));
  Term arg2(ctx.ip(2));
  ctx.deref(arg1);  // in case they are reg references
  ctx.deref(arg2);
  if (arg1 != arg2) {
    // immediate values must be exactly equal or we fail comparison
    if (Term::are_both_immed(arg1, arg2) ||
        !bif::are_terms_equal(ctx.vm_, arg1, arg2, true)) {
      return ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
    }
  }
  ctx.step_ip(3);
}
//  inline void opcode_is_ne_exact(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 44
//  }
inline void opcode_is_integer(Process* proc,
                              VMRuntimeContext& ctx) {  // opcode: 45
  // @spec is_integer Lbl Arg1
  // @doc Test the type of Arg1 and jump to Lbl if it is not an integer.
  Term arg(ctx.ip(1));
  ctx.deref(arg);
  if (!arg.is_integer()) {
    return ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
  }
  ctx.step_ip(2);
}
inline void opcode_is_float(Process* proc,
                            VMRuntimeContext& ctx) {  // opcode: 46
#if FEATURE_FLOAT
  G_TODO("is_float");
#else
  return ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
#endif
}
//  inline void opcode_is_number(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 47
//  }
inline void opcode_is_atom(Process* proc,
                           VMRuntimeContext& ctx) {  // opcode: 48
  // @spec is_atom Lbl Arg1
  // @doc Test the type of Arg1 and jump to Lbl if it is not an atom.
  Term arg(ctx.ip(1));
  ctx.deref(arg);
  if (!arg.is_atom()) {
    return ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
  }
  ctx.step_ip(2);
}
inline void opcode_is_pid(Process* proc, VMRuntimeContext& ctx) {  // opcode: 49
  // @spec is_pid Lbl Arg1
  // @doc Test the type of Arg1 and jump to Lbl if it is not a pid.
  Term arg(ctx.ip(1));
  ctx.deref(arg);
  if (!arg.is_pid()) {
    return ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
  }
  ctx.step_ip(2);
}
inline void opcode_is_reference(Process* proc,
                                VMRuntimeContext& ctx) {  // opcode: 50
  // @spec is_reference Lbl, Arg1
  // @doc Test the type of Arg1 and jump to Lbl if it is not a reference.
  Term t(ctx.ip(1));
  ctx.deref(t);
  if (t.is_reference() == false) {
    return ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
  }
  ctx.step_ip(2);
}
inline void opcode_is_port(Process* proc,
                           VMRuntimeContext& ctx) {  // opcode: 51
  // @spec is_port Lbl, Arg1
  // @doc Test the type of Arg1 and jump to Lbl if it is not a port.
  Term t(ctx.ip(1));
  ctx.deref(t);
  if (t.is_port() == false) {
    return ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
  }
  ctx.step_ip(2);
}
inline void opcode_is_nil(Process* proc, VMRuntimeContext& ctx) {  // opcode: 52
  // @spec is_nil Lbl Arg1
  // @doc Test the type of Arg1 and jump to Lbl if it is not nil.
  Term t(ctx.ip(1));
  ctx.deref(t);
  if (t.is_nil() == false) {
    return ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
  }
  ctx.step_ip(2);
}
inline void opcode_is_binary(Process* proc,
                             VMRuntimeContext& ctx) {  // opcode: 53
// @spec is_binary Lbl Arg1
// @doc Test the type of Arg1 and jump to Lbl if it is not a binary.
#if FEATURE_BINARIES
  G_TODO("is_binary");
#else
  return ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
#endif
}
//  inline void opcode_is_constant(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 54
//  }
inline void opcode_is_list(Process* proc,
                           VMRuntimeContext& ctx) {  // opcode: 55
  // @spec is_list Lbl Arg1
  // @doc Test the type of Arg1 and jump to Lbl if it is not a cons or nil.
  Term t(ctx.ip(1));
  ctx.deref(t);
  if (!t.is_cons() && !t.is_nil()) {
    return ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
  }
  ctx.step_ip(2);
}
inline void opcode_is_nonempty_list(Process* proc,
                                    VMRuntimeContext& ctx) {  // opcode: 56
  // @spec is_nonempty_list Lbl Arg1
  // @doc Test the type of Arg1 and jump to Lbl if it is not a cons.
  Term t(ctx.ip(1));
  ctx.deref(t);
  if (t.is_cons() == false) {
    return ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
  }
  ctx.step_ip(2);
}
inline void opcode_is_tuple(Process* proc,
                            VMRuntimeContext& ctx) {  // opcode: 57
  // @spec is_tuple Lbl Arg1
  // @doc Test the type of Arg1 and jump to Lbl if it is not a tuple.
  Term t(ctx.ip(1));
  ctx.deref(t);
  if (!t.is_tuple()) {
    return ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
  }
  ctx.step_ip(2);
}
inline void opcode_test_arity(Process* proc,
                              VMRuntimeContext& ctx) {  // opcode: 58
  // @spec test_arity Lbl Arg1 Arity
  // @doc Test the arity of (the tuple in) Arg1 and jump to Lbl if it is
  // not equal to Arity.
  Term t(ctx.ip(1));
  ctx.deref(t);
  Term t_arity(ctx.ip(2));
  ctx.deref(t_arity);
  if (!t.is_tuple() || t.tuple_get_arity() != t_arity.small_word()) {
    return ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
  }
  ctx.step_ip(3);
}
inline void opcode_select_val(Process* proc,
                              VMRuntimeContext& ctx) {  // opcode: 59
  // @spec select_val Arg FailLabel Destinations
  // @doc Jump to the destination label corresponding to Arg
  //      in the Destinations list, if no arity matches, jump to FailLabel.
  Term arg(ctx.ip(0));
  ctx.deref(arg);

  Term fail_label(ctx.ip(2));

  // TODO: binary search
  Word* elements = fail_label.boxed_get_ptr<Word>();
  Word dst_arity = ((Word*)elements)[0] / 2;
  for (Word i = 0; i < dst_arity; i++) {
    if (elements[i * 2 + 1] == arg.value()) {
      return ctx.jump(proc, ContinuationPointer(elements[i * 2 + 2]));
    }
  }
  ctx.jump(proc, ContinuationPointer(ctx.ip(1)));
}
inline void opcode_select_tuple_arity(Process* proc,
                                      VMRuntimeContext& ctx) {  // opcode: 60
  // @spec select_tuple_arity Tuple FailLabel Destinations
  // @doc Check the arity of the tuple Tuple and jump to the corresponding
  //      destination label, if no arity matches, jump to FailLabel.
  Term arg(ctx.ip(0));
  ctx.deref(arg);
  //    Word arity = arg.tuple_get_arity();

  // TODO: refactor this fun and select_val into shared fun
  Term fail_label(ctx.ip(2));

  // TODO: binary search
  Word* elements = fail_label.boxed_get_ptr<Word>();
  Word dst_arity = ((Word*)elements)[0] / 2;
  for (Word i = 0; i < dst_arity; i++) {
    if (elements[i * 2 + 1] == arg.value()) {
      return ctx.jump(proc, ContinuationPointer(elements[i * 2 + 2]));
    }
  }
  ctx.jump(proc, ContinuationPointer(ctx.ip(1)));
}
inline void opcode_jump(Process* proc, VMRuntimeContext& ctx) {  // opcode: 61
  return ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
}

inline void opcode_catch(Process* proc, VMRuntimeContext& ctx) {  // opcode: 62
  // @spec catch DstStackY Label
  // @doc Stores label address in given stack cell. Label points to catch_end
  //      section
  Term dst(ctx.ip(0));
  Term arg(ctx.ip(1));
  ctx.deref(arg);
  ctx.move(arg, dst);
  proc->catch_level_++;

  ctx.step_ip(2);
  // throw err::TODO("notimpl catch");
}

inline void opcode_catch_end(Process* proc,
                             VMRuntimeContext& ctx) {  // opcode: 63
  throw err::TODO("notimpl catch_end");
}

inline void opcode_move(Process* proc, VMRuntimeContext& ctx) {  // opcode: 64
  Term val(ctx.ip(0));
  ctx.deref(val);
  Term dst(ctx.ip(1));
  ctx.move(val, dst);
  ctx.step_ip(2);
}
inline void opcode_get_list(Process* proc,
                            VMRuntimeContext& ctx) {  // opcode: 65
  // @spec get_list  Source Head Tail
  // @doc  Get the head and tail (or car and cdr) parts of a list
  //       (a cons cell) from Source and put them into the registers
  //       Head and Tail.
  Term src(ctx.ip(0));
  ctx.deref(src);
  Term dst_head(ctx.ip(1));
  Term dst_tail(ctx.ip(2));
  ctx.move(src.cons_head(), dst_head);
  ctx.move(src.cons_tail(), dst_tail);
  ctx.step_ip(3);
}
inline void opcode_get_tuple_element(Process* proc,
                                     VMRuntimeContext& ctx) {  // opcode: 66
  // @spec get_tuple_element Source Element Destination
  // @doc  Get element number Element from the tuple in Source and put
  //       it in the destination register Destination.
  Term src(ctx.ip(0));
  ctx.deref(src);
  Term t_el(ctx.ip(1));
  ctx.deref(t_el);
  Word el = t_el.small_word();
  G_ASSERT(el >= 0 && el < src.tuple_get_arity());
  ctx.move(src.tuple_get_element(el), Term(ctx.ip(2)));
  ctx.step_ip(3);
}
//  inline void opcode_set_tuple_element(Process *proc, VMRuntimeContext &ctx) {
//  // opcode: 67
//  }
//  inline void opcode_put_string(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 68
//  }
inline void opcode_put_list(Process* proc,
                            VMRuntimeContext& ctx) {  // opcode: 69
  // @spec put_list H T Dst
  Term h(ctx.ip(0));
  Term t(ctx.ip(1));
  ctx.deref(h);
  ctx.deref(t);
  Term dst(ctx.ip(2));
  Term result = Term::allocate_cons(ctx.heap_, h, t);
  ctx.move(result, dst);
  ctx.step_ip(3);
}
inline void opcode_put_tuple(Process* proc,
                             VMRuntimeContext& ctx) {  // opcode: 70
  // @spec put_tuple Arity Dst
  // followed by Arity 'put N' commands
  Term t_arity(ctx.ip(0));
  Term dst(ctx.ip(1));
  ctx.step_ip(2);

  Word arity = t_arity.small_word();
  if (G_UNLIKELY(arity == 0)) {
    ctx.move(Term::make_zero_tuple(), dst);
    return;
  }

  Term* cells = (Term*)proc->heap_alloc(layout::Tuple::box_size(arity));
  Word index = 0;
  do {
    // assert that ip[0] is opcode 'put' skip it and read its argument
    Term value(ctx.ip(1));
    ctx.step_ip(2);
    ctx.deref(value);
    Std::fmt(tMagenta("put element "));
    value.println(ctx.vm_);
    layout::Tuple::element(cells, index) = value;
    index++;
  } while (index < arity);

  ctx.move(Term::make_tuple(cells, t_arity.small_word()), dst);
}
//  inline void opcode_put(Process *proc, VMRuntimeContext &ctx) { // opcode: 71
//  }
inline void opcode_badmatch(Process* proc,
                            VMRuntimeContext& ctx) {  // opcode: 72
  return ctx.raise(proc, atom::ERROR, atom::BADMATCH);
}
inline void opcode_if_end(Process* proc, VMRuntimeContext& ctx) {  // opcode: 73
  return ctx.raise(proc, atom::ERROR, atom::IF_CLAUSE);
}
inline void opcode_case_end(Process* proc,
                            VMRuntimeContext& ctx) {  // opcode: 74
  return ctx.raise(proc, atom::ERROR, atom::CASE_CLAUSE);
}
inline WantSchedule opcode_call_fun(Process* proc,
                                    VMRuntimeContext& ctx) {  // opcode: 75
  // @spec call_fun Arity
  // @doc Call a fun of arity Arity. Assume arguments in registers x(0) to
  // x(Arity-1) and that the fun is in x(Arity). Save the next instruction as
  // the return address in the CP register.
  Term t_arity(ctx.ip(0));
  G_ASSERT(t_arity.is_small());

  Word arity = t_arity.small_word();

  Term fun(ctx.regs_[arity]);
  if (fun.is_boxed_fun()) {
    BoxedFun* bf = fun.boxed_get_ptr<BoxedFun>();
    if (bf->get_arity() != arity + bf->get_num_free()) {
      // TODO: make tuple {badarity, f_args}
      ctx.raise(proc, atom::ERROR, atom::BADARITY);
      return WantSchedule::NextProcess;
    }
    // TODO: if bf.fe is null - unloaded fun
    Word num_free = bf->get_num_free();
    G_ASSERT(arity + num_free < erts::max_regs);
    std::copy(bf->frozen, bf->frozen + num_free, ctx.regs_ + arity);

    ctx.live = arity;
    ctx.set_cp(ctx.ip() + 1);
    ctx.set_ip(bf->fun_entry->code);
  } else if (fun.is_boxed_export()) {
    Export* ex = fun.boxed_get_ptr<Export>();
    G_ASSERT(arity == ex->mfa.arity);
    if (ex->is_bif()) {
      Term result = ctx.vm_.apply_bif(proc, arity, ex->code(), ctx.regs_);
      if (ctx.check_bif_error(proc) == CheckBifError::ErrorOccured) {
        return WantSchedule::NextProcess;
      }
      ctx.regs_[0] = result;
      ctx.inc_ip();
    } else {
      ctx.live = arity;
      ctx.set_cp(ctx.ip() + 1);
      ctx.set_ip(ex->code());
    }
  } else {
    // TODO: make tuple {badfun, f_args} (same as above)
    term::TupleBuilder tb(ctx.heap_, 2);
    tb.add(atom::BADFUN);
    tb.add(fun);
    ctx.raise(proc, atom::ERROR, tb.make_tuple());
  }
  return ctx.consume_reduction(proc);
}
//  inline void opcode_make_fun(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 76
//  }
inline void opcode_is_function(Process* proc,
                               VMRuntimeContext& ctx) {  // opcode: 77
  // @spec is_function Lbl Arg1
  // @doc Test the type of Arg1; jump to Lbl if it is not a function or closure.
  Term arg1(ctx.ip(1));
  ctx.deref(arg1);
  // Check if its fun at all
  if (!arg1.is_boxed_fun() && !arg1.is_boxed_export()) {
    return ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
  }
  ctx.step_ip(2);
}
inline WantSchedule opcode_call_ext_only(Process* proc,
                                         VMRuntimeContext& ctx) {  // opcode: 78
  // @spec call_ext_only Arity Label
  // Do a tail recursive call to the function at Label. Do not update CP.
  Term boxed_mfa(ctx.ip(1));
  Term a(ctx.ip(0));
  ctx.live = a.small_word();
  ctx.jump_ext(proc, boxed_mfa);
  return ctx.consume_reduction(proc);
}
//  inline void opcode_bs_start_match(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 79
//  }
//  inline void opcode_bs_get_integer(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 80
//  }
//  inline void opcode_bs_get_float(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 81
//  }
//  inline void opcode_bs_get_binary(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 82
//  }
//  inline void opcode_bs_skip_bits(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 83
//  }
//  inline void opcode_bs_test_tail(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 84
//  }
//  inline void opcode_bs_save(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 85
//  }
//  inline void opcode_bs_restore(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 86
//  }
//  inline void opcode_bs_init(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 87
//  }
//  inline void opcode_bs_final(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 88
//  }
//  inline void opcode_bs_put_integer(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 89
//  }
//  inline void opcode_bs_put_binary(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 90
//  }
//  inline void opcode_bs_put_float(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 91
//  }
//  inline void opcode_bs_put_string(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 92
//  }
//  inline void opcode_bs_need_buf(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 93
//  }
//  inline void opcode_fclearerror(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 94
//  }
//  inline void opcode_fcheckerror(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 95
//  }
//  inline void opcode_fmove(Process *proc, VMRuntimeContext &ctx) { // opcode:
//  96
//  }
//  inline void opcode_fconv(Process *proc, VMRuntimeContext &ctx) { // opcode:
//  97
//  }
//  inline void opcode_fadd(Process *proc, VMRuntimeContext &ctx) { // opcode:
//  98
//  }
//  inline void opcode_fsub(Process *proc, VMRuntimeContext &ctx) { // opcode:
//  99
//  }
//  inline void opcode_fmul(Process *proc, VMRuntimeContext &ctx) { // opcode:
//  100
//  }
//  inline void opcode_fdiv(Process *proc, VMRuntimeContext &ctx) { // opcode:
//  101
//  }
//  inline void opcode_fnegate(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 102
//  }
inline void opcode_make_fun2(Process* proc,
                             VMRuntimeContext& ctx) {  // opcode: 103
  // @spec make_fun2 LambdaT_index
  // @doc Produces a callable fun object
  Term boxed_fe(ctx.ip(0));
  FunEntry* fe = boxed_fe.boxed_get_ptr<FunEntry>();

  //    // TODO: mark boxed memory somehow so GC can recognize its size?
  //    Word *p8 = proc->heap_alloc(
  //          calculate_word_size(sizeof(BoxedFun)) + fe->num_free
  //          );
  //    // Assuming that regs have all values ready to be frozen (copied to
  //    closure)
  //    BoxedFun *p = fun::box_fun(fe, p8, proc->get_pid(), ctx.regs);

  //    ctx.regs[0] = FunObject::make(p);
  ctx.regs_[0] = fun::box_fun(proc->get_heap(), fe, proc->get_pid(), ctx.regs_);
  ctx.step_ip(1);
}
inline void opcode_try(Process* proc, VMRuntimeContext& ctx) {  // opcode: 104
  // @spec try Arg1 Label
  throw err::TODO("notimpl try");
}
//  inline void opcode_try_end(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 105
//  }
inline void opcode_try_case(Process* proc,
                            VMRuntimeContext& ctx) {  // opcode: 106
  // @spec try_case Arg1
  throw err::TODO("notimpl try_case");
}
//  inline void opcode_try_case_end(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 107
//  }
//  inline void opcode_raise(Process *proc, VMRuntimeContext &ctx) { // opcode:
//  108
//  }
//  inline void opcode_bs_init2(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 109
//  }
//  inline void opcode_bs_bits_to_bytes(Process *proc, VMRuntimeContext &ctx) {
//  // opcode: 110
//  }
//  inline void opcode_bs_add(Process *proc, VMRuntimeContext &ctx) { // opcode:
//  111
//  }

static WantSchedule after_apply(Process* proc,
                                VMRuntimeContext& ctx,
                                Word arity,
                                Either<Word*, Term> res) {
  // Check error
  if (ctx.check_bif_error(proc) == CheckBifError::ErrorOccured) {
    return WantSchedule::NextProcess;
  }
  // What to do with apply result, is it code pointer to jump to or a bif result
  if (res.is_left()) {
    // Imitate a call
    ctx.live = arity;
    ctx.set_cp(ctx.ip());
    ctx.set_ip(res.left());
    return ctx.consume_reduction(proc);
  }
  // Or we already know the result, no call is happening
  ctx.regs_[0] = res.right();
  return ctx.consume_reduction(proc);
}

inline WantSchedule opcode_apply(Process* proc,
                                 VMRuntimeContext& ctx) {  // opcode: 112
  // @spec apply Arity [x[0..Arity-1]=args, x[arity]=m, x[arity+1]=f]
  Term arity_as_term(ctx.ip(0));
  Word arity = arity_as_term.small_word();
  Term mod = ctx.regs_[arity];
  Term fun = ctx.regs_[arity + 1];

  ctx.swap_out_partial(proc);
  Either<Word*, Term> res = proc->apply(mod, fun, arity_as_term);
  ctx.swap_in_partial(proc);

  ctx.inc_ip();  // consume Arity arg
  return after_apply(proc, ctx, arity, res);
}

inline WantSchedule opcode_apply_last(Process* proc,
                                      VMRuntimeContext& ctx) {  // opcode: 113
  // @spec apply_last _ Arity [x[0..Arity-1]=args, x[arity]=m, x[arity+1]=f]
  Term arity_as_term(ctx.ip(0));
  Word arity = arity_as_term.small_word();
  Term mod = ctx.regs_[arity];
  Term fun = ctx.regs_[arity + 1];

  ctx.swap_out_partial(proc);
  Either<Word*, Term> res = proc->apply(mod, fun, arity_as_term);
  ctx.swap_in_partial(proc);

  // Check error
  if (ctx.check_bif_error(proc) == CheckBifError::ErrorOccured) {
    return WantSchedule::NextProcess;
  }

  ctx.step_ip(2);  // consume (_,Arity) args
  return after_apply(proc, ctx, arity, res);
}

inline void opcode_is_boolean(Process* proc,
                              VMRuntimeContext& ctx) {  // opcode: 114
  // @spec is_boolean Lbl Arg1
  // @doc Test the type of Arg1 and jump to Lbl if it is not a Boolean.
  Term arg1(ctx.ip(1));
  ctx.deref(arg1);
  if (arg1 != atom::TRUE && arg1 != atom::FALSE) {
    return ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
  }
  ctx.inc_ip();
}

inline void opcode_is_function2(Process* proc,
                                VMRuntimeContext& ctx) {  // opcode: 115
  // @spec is_function2 Lbl Arg1 Arity
  // @doc Test the type of Arg1 and jump to Lbl if it is not a function
  // of arity Arity.
  Term arg1(ctx.ip(1));
  ctx.deref(arg1);
  Term arity(ctx.ip(2));
  ctx.deref(arity);
  // Check if its fun at all
  if (arg1.is_boxed_fun()) {
    // check arity
    BoxedFun* bf = arg1.boxed_get_ptr<BoxedFun>();
    if (arity.small_word() + bf->get_num_free() != bf->get_arity()) {
      return ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
    }
  } else if (arg1.is_boxed_export()) {
    // check arity
    Export* ex = arg1.boxed_get_ptr<Export>();
    if (arity.small_word() != ex->mfa.arity) {
      return ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
    }
  } else {
    return ctx.jump(proc, ContinuationPointer(ctx.ip(0)));
  }

  ctx.step_ip(3);
}
//  inline void opcode_bs_start_match2(Process *proc, VMRuntimeContext &ctx) {
//  // opcode: 116
//  }
//  inline void opcode_bs_get_integer2(Process *proc, VMRuntimeContext &ctx) {
//  // opcode: 117
//  }
//  inline void opcode_bs_get_float2(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 118
//  }
//  inline void opcode_bs_get_binary2(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 119
//  }
//  inline void opcode_bs_skip_bits2(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 120
//  }
//  inline void opcode_bs_test_tail2(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 121
//  }
//  inline void opcode_bs_save2(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 122
//  }
//  inline void opcode_bs_restore2(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 123
//  }

inline WantSchedule opcode_gc_bif1(Process* proc,
                                   VMRuntimeContext& ctx) {  // opcode: 124
  // @spec gc_bif1 Lbl Live Bif Arg Reg
  // @doc Call the bif Bif with the argument Arg, and store the result in Reg.
  // On failure jump to Lbl. Do a garbage collection if necessary to allocate
  // space on the heap for the result (saving Live number of X registers).
  Term boxed_fun(ctx.ip(2));
  Term arg1(ctx.ip(3));
  ctx.deref(arg1);
  Term result_dst(ctx.ip(4));

  MFArity* mfa = boxed_fun.boxed_get_ptr<MFArity>();
  bif1_fn fn1 = (bif1_fn)ctx.vm_.find_bif(*mfa);
  G_ASSERT(fn1);

  ctx.swap_out(proc);
  Term result = fn1(proc, arg1);
  ctx.swap_in(proc);

  if (ctx.check_bif_error(proc) == CheckBifError::ErrorOccured) {
    return WantSchedule::NextProcess;
  }
  ctx.move(result, result_dst);
  ctx.step_ip(5);
  return ctx.consume_reduction(proc);
}

inline WantSchedule opcode_gc_bif2(Process* proc,
                                   VMRuntimeContext& ctx) {  // opcode: 125
  // @spec gc_bif2 Lbl Live Bif Arg1 Arg2 Reg
  // @doc Call the bif Bif with the arguments Arg1 and Arg2, and store the
  // result in Reg. On failure jump to Lbl. Do a garbage collection if
  // necessary to allocate space on the heap for the result (saving Live
  // number of X registers).
  Term boxed_fun(ctx.ip(2));
  Term arg1(ctx.ip(3));
  Term arg2(ctx.ip(4));
  ctx.deref(arg1);
  ctx.deref(arg2);
  Term result_dst(ctx.ip(5));

  MFArity* mfa = boxed_fun.boxed_get_ptr<MFArity>();
  bif2_fn fn2 = (bif2_fn)ctx.vm_.find_bif(*mfa);
  G_ASSERT(fn2);

  ctx.swap_out(proc);
  Term result = fn2(proc, arg1, arg2);
  ctx.swap_in(proc);

  if (ctx.check_bif_error(proc) == CheckBifError::ErrorOccured) {
    return WantSchedule::NextProcess;
  }
  ctx.move(result, result_dst);
  ctx.step_ip(6);
  return ctx.consume_reduction(proc);
}
//  inline void opcode_bs_final2(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 126
//  }
//  inline void opcode_bs_bits_to_bytes2(Process *proc, VMRuntimeContext &ctx) {
//  // opcode: 127
//  }
//  inline void opcode_put_literal(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 128
//  }
//  inline void opcode_is_bitstr(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 129
//  }
//  inline void opcode_bs_context_to_binary(Process *proc, VMRuntimeContext
//  &ctx) { // opcode: 130
//  }
//  inline void opcode_bs_test_unit(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 131
//  }
//  inline void opcode_bs_match_string(Process *proc, VMRuntimeContext &ctx) {
//  // opcode: 132
//  }
//  inline void opcode_bs_init_writable(Process *proc, VMRuntimeContext &ctx) {
//  // opcode: 133
//  }
//  inline void opcode_bs_append(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 134
//  }
//  inline void opcode_bs_private_append(Process *proc, VMRuntimeContext &ctx) {
//  // opcode: 135
//  }
inline void opcode_trim(Process* proc, VMRuntimeContext& ctx) {  // opcode: 136
  // @spec trim N Remaining
  // @doc Reduce the stack usage by N words, keeping the CP on the top.
  Term n(ctx.ip(0));
  auto masq_cp = ctx.stack().pop();
  ctx.stack().drop_n(n.small_word());
  ctx.stack().push(masq_cp);
  ctx.step_ip(2);
}
//  inline void opcode_bs_init_bits(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 137
//  }
//  inline void opcode_bs_get_utf8(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 138
//  }
//  inline void opcode_bs_skip_utf8(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 139
//  }
//  inline void opcode_bs_get_utf16(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 140
//  }
//  inline void opcode_bs_skip_utf16(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 141
//  }
//  inline void opcode_bs_get_utf32(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 142
//  }
//  inline void opcode_bs_skip_utf32(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 143
//  }
//  inline void opcode_bs_utf8_size(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 144
//  }
//  inline void opcode_bs_put_utf8(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 145
//  }
//  inline void opcode_bs_utf16_size(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 146
//  }
//  inline void opcode_bs_put_utf16(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 147
//  }
//  inline void opcode_bs_put_utf32(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 148
//  }
//  inline void opcode_on_load(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 149
//  }
//
// R14A
//
inline void opcode_recv_mark(Process* proc,
                             VMRuntimeContext& ctx) {  // opcode: 150
  // @spec recv_mark Label
  // @doc  Save the end of the message queue and the address of the label
  // Label so that a recv_set instruction can start scanning the inbox from
  // this position.
  proc->mailbox().mark_position(ctx.ip(0));
  ctx.inc_ip();
}
inline void opcode_recv_set(Process* proc,
                            VMRuntimeContext& ctx) {  // opcode: 151
  // @spec recv_set Label
  // @doc Check that the saved mark points to Label and set the save pointer
  // in the message queue to the last position of the message queue saved by
  // the recv_mark instruction.
  proc->mailbox().set_to_marked(ctx.ip(0));
  ctx.inc_ip();
  // ip+1 points to the next iop, supposedly loop_rec
}
//  inline void opcode_gc_bif3(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 152
//  }
//  inline void opcode_line(Process *proc, VMRuntimeContext &ctx) { // opcode:
//  153
//  }
//  inline void opcode_put_map_assoc(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 154
//  }
//  inline void opcode_put_map_exact(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 155
//  }
//  inline void opcode_is_map(Process *proc, VMRuntimeContext &ctx) { // opcode:
//  156
//  }
//  inline void opcode_has_map_fields(Process *proc, VMRuntimeContext &ctx) { //
//  opcode: 157
//  }
inline void opcode_get_map_elements(Process* proc,
                                    VMRuntimeContext& ctx) {  // opcode: 158
  // This must be implemented for assert_address_makes_sense/2 to know
  // whether a jump address is valid or random garbage. We throw up here
  throw err::TODO("notimpl get_map_elements");
}

inline WantSchedule opcode_normal_exit_(Process* proc,
                                        VMRuntimeContext& ctx) {  // opcode: 158
  // This must be implemented for assert_address_makes_sense/2 to know
  // whether a jump address is valid or random garbage. We throw up here
  throw err::TODO("notimpl exit");
}

inline WantSchedule opcode_apply_mfargs_(
    Process* proc,
    VMRuntimeContext& ctx) {  // opcode: N+1
  // @spec apply_mfargs_, regs contain m,f,[args]
  auto arg_regs = &proc->get_runtime_ctx().arg_regs_[0];
  Term mod = arg_regs[0];
  Term fun = arg_regs[1];
  Term args = arg_regs[2];
  G_ASSERT(args.is_list());

  Std::fmt(tMagenta("apply_mfargs_") ": ");
  mod.print(ctx.vm_);
  Std::fmt(":");
  fun.print(ctx.vm_);
  args.print(ctx.vm_);
  Std::fmt("\n");

  ctx.swap_out_partial(proc);
  Either<Word*, Term> res = proc->apply(mod, fun, args);
  ctx.swap_in_partial(proc);

  auto arity_result = bif::length(args);
  G_ASSERT(arity_result.is_proper == true);

  return after_apply(proc, ctx, arity_result.length, res);
}

}  // ns impl
}  // ns gluon
