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

  m_ip.module = mod;
  auto r_result = mod->resolve_function(f, arity);
  G_RETURN_IF_ERROR(r_result);
  m_ip.offset = r_result.get_result();
  return success();
}

void *Process::vm_fetch_instr()
{
  return reinterpret_cast<void *>(m_ip.next_word());
}

word_t gleam_ptr_t::next_word() {
  G_ASSERT(is_good());
  auto word = module->fetch_word(offset);
  offset.value++;
  return word;
}

} // ns gluon
