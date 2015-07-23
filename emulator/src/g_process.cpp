#include "g_process.h"
#include "g_codeserver.h"
#include "g_code.h"
#include "g_module.h"
#include "g_vm.h"
#include "g_error.h"

namespace gluon {

MaybeError Process::call(Term m, Term f, word_t arity, Term args)
{
  G_ASSERT(this);

  auto mod_result = CodeServer::find_module(m, CodeServer::LOAD_IF_NOT_FOUND);
  G_RETURN_IF_ERROR(mod_result);
  Module *mod = mod_result.get_result();

//  if (m_ctx.mod) {
//    // push current ip if it wasn't null (initial entry call)
//    m_stack.push(Term::make_boxed(m_ctx.ip));
//  }

//  m_ctx.mod = mod;
  auto find_result = mod->resolve_function(f, arity);
  G_RETURN_IF_ERROR(find_result);
  m_ctx.ip = find_result.get_result();
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



} // ns gluon
