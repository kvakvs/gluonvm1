#include "g_process.h"
#include "g_code_server.h"
#include "g_code.h"
#include "g_module.h"
#include "g_vm.h"
#include "g_error.h"
#include "g_predef_atoms.h"
#include "g_term_helpers.h"

#if G_TEST
#include <fructose/fructose.h>
#endif

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



//word_t *Process::get_code_base() const {
//  return m_module->m_code.data();
//}
//word_t *Process::get_ip() const {
//  return m_ctx.mod->m_code.data() + m_ip.offset.value;
//}

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
  other->incoming_send(dst_value);
  // TODO: also inform scheduler?
}

void Process::incoming_send(Term value)
{
  m_mbox.push_back(value);
}

Term Process::msg_current()
{
  if (m_mbox.empty()) {
    return NONVALUE;
  }
  if (m_mbox_ptr == m_mbox.end()) {
    m_mbox_ptr = m_mbox.begin();
  }
  return *m_mbox_ptr;
}

void Process::msg_remove()
{
  if (m_mbox_ptr == m_mbox.end()) {
    m_mbox_ptr = m_mbox.begin();
  } else {
    m_mbox_ptr = m_mbox.erase(m_mbox_ptr);
  }
}

void Process::msg_next()
{
  if (m_mbox_ptr == m_mbox.end()) {
    m_mbox_ptr = m_mbox.begin();
  } else {
    m_mbox_ptr++;
  }
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

//
//====================================
//
#if G_TEST
} // ns gluon

#include "bif/g_bif_misc.h"
#pragma clang diagnostic ignored "-Wweak-vtables"
namespace gluon {
struct process_test_t: public fructose::test_base<process_test_t>
{

  void test_process_stack(const std::string &tn) {
    ProcessStack s;
    s.push(Term::make_small_u(1)); // y[3]
    s.push_n_nils(3);              // y[0 1 2]
    s.push(Term::make_small_u(2)); // y[-1]
    s.set_y(1, NONVALUE);
    fructose_assert(s.get_y(0).is_nil());
    fructose_assert(s.get_y(1).is_non_value());
    fructose_assert(s.get_y(2).is_nil());
    fructose_assert(s.get_y(3) == Term::make_small_u(1));
  }

}; // struct

void process_test(int argc, const char *argv[]) {
  process_test_t tests;
  tests.add_test("process_stack", &process_test_t::test_process_stack);
  tests.run(argc, const_cast<char **>(argv));
}

#endif // TEST

} // ns gluon
