#include "g_process.h"
#include "g_code_server.h"
#include "g_code.h"
#include "g_module.h"
#include "g_vm.h"
#include "g_error.h"
#include "g_predef_atoms.h"
#include "g_term_helpers.h"

namespace gluon {

Process::Process(VM &vm, Term gleader): vm_(vm), gleader_(gleader) {
  prio_ = atom::NORMAL;
}

void Process::jump_to_mfa(MFArity &mfa)
{
  G_ASSERT(this);

  auto mod = vm_.codeserver().find_module(this, mfa.mod,
                                       code::FindModule::LoadIfNotFound);

  Export *exp = mod->find_export(mfa.as_funarity());
  if (!exp) {
    throw err::Process("undef function");
  }
  if (exp->is_bif()) {
    // Run the bif, hope it returns control soon
    vm_.apply_bif(this, exp->mfa.arity, exp->bif_fn(), ctx_.regs);
    mfa.println(vm_);
    throw err::Process("jump to a bif");
  }

  ctx_.ip = exp->code();
  Std::fmt("Process::jump_to_mfa -> " FMT_0xHEX "\n", (Word)ctx_.ip);
  G_ASSERT(ctx_.ip > 0);
}


Term Process::spawn(MFArity &mfa, Term *args) {
  // Check that we aren't on any scheduler yet
  G_ASSERT(false == pid_.is_pid());

  initial_call_ = mfa;
  jump_to_mfa(mfa);

  std::copy(args, args+mfa.arity, ctx_.regs);
  ctx_.live = mfa.arity;

  // TODO: set context cp to some special exit function or handle exit another way

  vm_.scheduler().add_new_runnable(this);
  return get_pid();
}

Term Process::bif_error(Term error_tag, Term reason)
{
  term::TupleBuilder tb(get_heap(), 2);
  tb.add(error_tag);
  tb.add(reason);
  bif_err_reason_ = tb.make_tuple();
  return the_non_value;
}

Term Process::bif_error(Term reason)
{
  bif_err_reason_ = reason;
  return the_non_value;
}

Term Process::bif_error(Term reason, const char *str)
{
  Term err = term::build_string(get_heap(), str);
  return bif_error(reason, err);
}

Term Process::bif_badarg(Term reason)
{
  return bif_error(atom::BADARG, reason);
}

Term Process::bif_badarg()
{
  return bif_error(atom::BADARG);
}

void Process::msg_send(Term pid, Term value)
{
  // TODO: send to tuple {dst,node}, and to registered atom, and to port
  G_ASSERT(pid.is_pid());

  Process *other = vm_.scheduler().find(pid);
  if (!other) {
    Std::fmt("msg_send pid not found: ");
    pid.println(vm_);
    return;
  }
  // Clone local value to value on remote heap
  Term dst_value = proc::copy_one_term(vm_, other->get_heap(), value);
  other->mailbox().on_incoming(dst_value);
  vm_.scheduler().on_new_message(other); // wake up receiver
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
#endif //0

} // ns
