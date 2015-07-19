#include "g_process.h"
#include "g_codeserver.h"
#include "g_code.h"
#include "g_module.h"

namespace gluon {

MaybeError Process::call(Term m, Term f, word_t arity, Term args)
{
  G_ASSERT(this);

  auto mod = CodeServer::find_module(m);
  if (!mod) {
    return "module not found";
  }

  if (m_ip.module) {
    // push current ip if it wasn't null (initial entry call)
    m_call_stack.push_back(m_ip);
  }

  m_ip.module = mod;
  auto r_result = mod->resolve_function(f, arity);
  G_RETURN_IF_ERROR(r_result);
  m_ip.offset = r_result.get_result();
  return success();
}

word_t *Process::get_code_base() const {
  return m_ip.module->m_code.data();
}
word_t *Process::get_ip() const {
  return m_ip.module->m_code.data() + m_ip.offset.value;
}



} // ns gluon
