#include "g_vm.h"
#include "g_codeserver.h"
#include "g_sys_fs.h"
#include "g_sys_mem.h"
#include "g_dist.h"
#include "g_heap.h"
#include "g_process.h"

namespace gluon {

str_atom_map_t VM::g_atoms;
atom_str_map_t VM::g_atoms_reverse;
word_t VM::g_atom_counter = 0;
Node *VM::g_this_node = nullptr;
const void **VM::g_opcode_labels;

void VM::init()
{
  g_this_node = new Node;
  vm_loop(nullptr); // initialize labels
  CodeServer::init();
}

MaybeError VM::load_module(const Str &filename)
{
  fs::File modf;
  auto open_result = modf.open(filename);
  G_RETURN_IF_ERROR(open_result);

  word_t size = modf.size().get_result();

  auto alloc_result = mem::alloc_bytes(size);
  G_RETURN_IF_ERROR_UNLIKELY(alloc_result);

  auto bytes = alloc_result.get_result();
  auto r_result = modf.read(bytes, size);
  G_RETURN_IF_ERROR(r_result)

  CodeServer::load_module(Term::make_nil(), bytes, size);
  return success();
}

Term VM::to_atom(const Str &s)
{
  Term a = to_existing_atom(s);
  return a.is_nil() ? new_atom(s) : a;
}

Term VM::to_existing_atom(const Str &s) {
  auto iter = g_atoms.find(s);
  if (iter != g_atoms.end()) {
    return iter->second;
  }
  return Term::make_nil();
}

Term VM::new_atom(const Str &s) {
  auto iter = g_atoms.find(s);
  if (iter != g_atoms.end()) {
    G_FAIL("atom exists")
    G_IF_NODEBUG(return Term::make_nil());
  }

  Term new_a = Term::make_atom(g_atom_counter);
  g_atoms[s] = new_a;
  g_atoms_reverse[new_a] = s;
  g_atom_counter++;
  return new_a;
}

Str VM::find_atom(Term a)
{
  auto iter = g_atoms_reverse.find(a);
  if (iter != g_atoms_reverse.end()) {
    return iter->second;
  }
  return "";
}

Node *VM::dist_this_node() {
#if FEATURE_ERL_DIST
  G_TODO("implement Node and this node variable")
#endif
  return g_this_node;
}

// For now all heaps are located in normal C++ heap
Heap *VM::get_heap(VM::heap_t) {
  return nullptr;
}


#include "g_vm_loop.inl"

// Cool LLVM blockaddress extension, could not make it work, yet
// http://blog.llvm.org/2010/01/address-of-label-and-indirect-branches.html
//#include "g_vm_jump_table.inl"

} // ns gluon
