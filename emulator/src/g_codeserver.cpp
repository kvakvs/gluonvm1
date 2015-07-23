#include "g_codeserver.h"
#include "g_module.h"
#include "g_sys_fs.h"
#include "g_heap.h"
#include "g_vm.h"

namespace gluon {

mod_map_t CodeServer::g_modules;
List<Str> CodeServer::g_search_path;

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

MaybeError CodeServer::load_module(Term name)
{
  // Scan for locations where module file can be found
  Str mod_filename = name.atom_str() + ".S.gleam";

  for (const Str &dir: g_search_path) {
    Str path = dir + "/" + mod_filename;
    if (fs::exists(path)) {
      fs::File f;
      auto open_result = f.open(path);
      G_RETURN_IF_ERROR_UNLIKELY(open_result);
      word_t  size = f.size();
      Heap    *heap = VM::get_heap(VM::HEAP_CODE);
      u8_t    *tmp_buffer = Heap::alloc_bytes(heap, size);
      f.seek(0);
      f.read(tmp_buffer, size);

      auto    result = load_module(name, tmp_buffer, size);
      Heap::free_bytes(heap, tmp_buffer);
      return result;
    }
  }
  return "module not found";
}

Result<Module *> CodeServer::find_module(Term m, find_opt_t load)
{
  auto iter = g_modules.find(m);
  if (iter == g_modules.end()) {
    if (load == CodeServer::FIND_EXISTING) {
      return error<Module *>("function not found");
    } else {
      auto res = load_module(m);
      G_RETURN_REWRAP_IF_ERROR(res, Module *);
      return success(g_modules[m]);
    }
  }
  return success(iter->second);
}

void CodeServer::path_append(const Str &p)
{
  g_search_path.push_back(p);
}

void CodeServer::path_prepend(const Str &p)
{
  g_search_path.push_front(p);
}

} // ns gluon
