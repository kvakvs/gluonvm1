#include "g_process.h"
#include "g_code_server.h"
#include "g_code.h"
#include "g_module.h"
#include "g_vm.h"
#include "g_error.h"
#include "g_predef_atoms.h"
#include "g_term_helpers.h"

namespace gluon {

Process::Process(VM& vm, Term gleader) : vm_(vm), gleader_(gleader) {
  prio_ = atom::NORMAL;
}

Term Process::spawn(MFArity& mfa, Term* args) {
  // Check that we aren't on any scheduler yet
  G_ASSERT(false == pid_.is_pid());

  initial_call_ = mfa;

  // Set regs to M,F,A and jump to apply_mfargs_
  ctx_.regs[0] = mfa.mod;
  ctx_.regs[1] = mfa.fun;
  ctx_.regs[2] = term::build_list(get_heap(), args, args+mfa.arity);
  ctx_.live = 3;

  // Precondition: Registers should be set to execute apply call
  ctx_.cp = vm_.premade_instr(PremadeIndex::Normal_exit_);
  ctx_.ip = vm_.premade_instr(PremadeIndex::Apply_mfargs_);

  Std::fmt("Process::jump_to_mfa -> " FMT_0xHEX "\n", (Word)ctx_.ip);

  vm_.scheduler().add_new_runnable(this);
  return get_pid();
}

Term Process::bif_error(Term error_tag, Term reason) {
  term::TupleBuilder tb(get_heap(), 2);
  tb.add(error_tag);
  tb.add(reason);
  bif_err_reason_ = tb.make_tuple();
  return the_non_value;
}

Term Process::bif_error(Term reason) {
  bif_err_reason_ = reason;
  return the_non_value;
}

Term Process::bif_error(Term reason, const char* str) {
  Term err = term::build_string(get_heap(), str);
  return bif_error(reason, err);
}

Term Process::bif_badarg(Term reason) {
  return bif_error(atom::BADARG, reason);
}

Term Process::bif_badarg() {
  return bif_error(atom::BADARG);
}

void Process::msg_send(Term pid, Term value) {
  // TODO: send to tuple {dst,node}, and to registered atom, and to port
  G_ASSERT(pid.is_pid());

  Process* other = vm_.scheduler().find(pid);
  if (!other) {
    Std::fmt("msg_send pid not found: ");
    pid.println(vm_);
    return;
  }
  // Clone local value to value on remote heap
  Term dst_value = proc::copy_one_term(vm_, other->get_heap(), value);
  other->mailbox().on_incoming(dst_value);
  vm_.scheduler().on_new_message(other);  // wake up receiver
}

#if 0
#if G_DEBUG
void ProcessStack::println()
{
  Std::fmt("STACK[" FMT_UWORD "words]: ", size());
  if (size() > 0) {
    Std::fmt("[-1]=");
    cells.back().print();
    Std::fmt("; ");
  }
  if (size() > 1) {
    for (Word i = 0; i < size()-1; i++) {
      Std::fmt("[" FMT_UWORD "]=", i);
      get_y(i).print();
      Std::fmt("; ");
    }
  }
  Std::puts();
}
#endif
#endif  // 0

}  // ns
