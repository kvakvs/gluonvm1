#include "g_process.h"
#include "g_codeserver.h"
#include "g_code.h"
#include "g_module.h"
#include "g_vm.h"
#include "g_error.h"
#include "g_predef_atoms.h"

namespace gluon {

Process::Process(Term gleader): m_group_leader(gleader) {
  m_priority = atom::NORMAL;
}

MaybeError Process::jump_to_mfa(mfarity_t &mfa)
{
  G_ASSERT(this);

  auto mod_result = CodeServer::find_module(mfa.mod, CodeServer::LOAD_IF_NOT_FOUND);
  G_RETURN_IF_ERROR(mod_result);
  Module *mod = mod_result.get_result();

  auto find_result = mod->resolve_function(mfa.fun, mfa.arity);
  G_RETURN_IF_ERROR(find_result);

  auto ip = find_result.get_result();
  G_ASSERT(ip);

  m_ctx.ip = ip;
  printf("Process::jump_to_mfa -> 0x%zx\n", (word_t)m_ctx.ip);
  return success();
}

Heap *Process::get_heap()
{
  return VM::get_heap(VM::HEAP_PROCESS);
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

  // TODO: copy args to registers

  // TODO: set context cp to some special exit function or handle exit another way

  auto add_result = VM::get_scheduler()->add_runnable(this);
  G_RETURN_REWRAP_IF_ERROR(add_result, Term);

  return success(get_pid());
}

Term Process::bif_error(Term reason)
{
  m_bif_error_reason = reason;
  return Term::make_non_value();
}

//void ProcessStack::println()
//{
//  if (size() < 2) {
//    return;
//  }
//  printf("STACK: ");
//  for (word_t i = 0; i < size()-1; i++) {
//    printf("[%zu]=", i);
//    get_y(i).print();
//    printf("; ");
//  }
//  puts("");
//}


} // ns gluon
