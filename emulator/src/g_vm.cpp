#include "g_vm.h"
#include "g_codeserver.h"
#include "g_sys_fs.h"
#include "g_sys_mem.h"

namespace gluon {

str_atom_map_t VM::g_atoms;
atom_str_map_t VM::g_atoms_reverse;

void VM::init()
{
  CodeServer::init();
}

MaybeError VM::load_module(const Str &filename)
{
  fs::File modf(filename);
  auto size_result = modf.size();
  G_RETURN_IF_ERROR(size_result);

  word_t size = size_result.get_result();
  auto alloc_result = mem::alloc_bytes(size);
  G_RETURN_IF_ERROR_UNLIKELY(alloc_result);

  auto r_result = modf.read(alloc_result.get_result(), size);
  G_RETURN_IF_ERROR_UNLIKELY(r_result)

  return success();
}

} // ns gluon
