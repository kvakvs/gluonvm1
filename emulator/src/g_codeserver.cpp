#include "g_codeserver.h"
#include "g_module.h"

namespace gluon {

mod_map_t CodeServer::g_modules;

void CodeServer::init() {
}

MaybeError CodeServer::load_module(Term name_atom, const u8_t *bytes, word_t size)
{
  auto lm_result = load_module_internal(name_atom, bytes, size);
  G_RETURN_IF_ERROR(lm_result);

  // TODO: module versions for hot code loading
  // TODO: free if module already existed
  Module *m = lm_result.get_result();
  g_modules[m->get_name()] = m;

  return success();
}

Module *CodeServer::find_module(Term m)
{
  auto iter = g_modules.find(m);
  if (iter == g_modules.end()) {
    return nullptr;
  }
  return iter->second;
}

} // ns gluon
