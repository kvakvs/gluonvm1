#include "g_process.h"
#include "g_code_server.h"
#include "g_code.h"
#include "g_module.h"
#include "g_vm.h"
#include "g_error.h"
#include "g_predef_atoms.h"
#include "g_term_helpers.h"

namespace gluon {

Process::Process(Term gleader): m_group_leader(gleader) {
  m_priority = atom::NORMAL;
}

MaybeError Process::jump_to_mfa(mfarity_t &mfa)
{
  G_ASSERT(this);

  auto mod_result = VM::get_cs()->find_module(this, mfa.mod,
                                              code::LOAD_IF_NOT_FOUND);
  G_RETURN_IF_ERROR(mod_result);
  Module *mod = mod_result.get_result();

  auto find_result = mod->resolve_function(mfa.fun, mfa.arity);
  G_RETURN_IF_ERROR(find_result);

  auto ip = find_result.get_result();
  G_ASSERT(ip);

  m_ctx.ip = ip;
  printf("Process::jump_to_mfa -> " FMT_0xHEX "\n", (word_t)m_ctx.ip);
  return success();
}


Result<Term> Process::spawn(mfarity_t &mfa, Term *args) {
  // Check that we aren't on any scheduler yet
  G_ASSERT(false == m_pid.is_pid());

  m_init_call = mfa;
  auto j_result = jump_to_mfa(mfa);
  G_RETURN_REWRAP_IF_ERROR(j_result, Term);

  std::copy(args, args+mfa.arity, m_ctx.regs);
  m_ctx.live = mfa.arity;

  // TODO: set context cp to some special exit function or handle exit another way

  auto add_result = VM::get_scheduler()->add_new_runnable(this);
  G_RETURN_REWRAP_IF_ERROR(add_result, Term);

  return success(get_pid());
}

Term Process::bif_error(Term error_tag, Term reason)
{
  term::TupleBuilder tb(get_heap(), 2);
  tb.add(error_tag);
  tb.add(reason);
  m_bif_error_reason = tb.make_tuple();
  return NONVALUE;
}

Term Process::bif_error(Term reason)
{
  m_bif_error_reason = reason;
  return NONVALUE;
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

void Process::msg_send(Term pid, Term value)
{
  // TODO: send to tuple {dst,node}, and to registered atom, and to port
  G_ASSERT(pid.is_pid());

  Process *other = VM::get_scheduler()->find(pid);
  if (!other) {
    printf("msg_send pid not found: ");
    pid.println();
    return;
  }
  // Clone local value to value on remote heap
  Term dst_value = proc::copy_one_term(other->get_heap(), value);
  other->mailbox().on_incoming(dst_value);
  VM::get_scheduler()->on_new_message(other); // wake up receiver
}

#if 0
#if G_DEBUG
void ProcessStack::println()
{
  printf("STACK[" FMT_UWORD "words]: ", size());
  if (size() > 0) {
    printf("[-1]=");
    cells.back().print();
    printf("; ");
  }
  if (size() > 1) {
    for (word_t i = 0; i < size()-1; i++) {
      printf("[" FMT_UWORD "]=", i);
      get_y(i).print();
      printf("; ");
    }
  }
  puts("");
}
#endif
#endif //0
