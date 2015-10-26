#include "g_code_server.h"
#include "g_module.h"
#include "platf/gsys_file.h"
#include "g_heap.h"
#include "g_vm.h"
#include "g_process.h"

namespace gluon {
namespace code {

//mod_map_t Server::g_modules;
//List<Str> Server::g_search_path;

//void Server::init() {
//}

void Server::load_module(Process *proc,
                        Term name_atom, ArrayView<const Uint8> data)
{
  // TODO: module versions for hot code loading
  // TODO: free if module already existed, check module usage by processes
  Module *m = load_module_internal(proc->get_heap(), name_atom, data);
  modules_[m->get_name()] = m;

  // assume that mod already registered own functions in own fun index
  // code to fun/arity mapping should be updated on loading stage
  if (feature_code_ranges) {
    auto range = m->get_code_range();
    mod_index_.add(range, m);
  }
}

void Server::load_module(Process *proc, Term name)
{
  // Scan for locations where module file can be found
  Str mod_filename(name.atom_str(vm_));
  mod_filename += ".beam";

  for (const Str &dir: search_path_) {
    Str path(dir);
    path += "/";
    path += mod_filename;

    if (fs::exists(path)) {
      fs::File f;
      f.open(path);

      Word  size = f.size();
      erts::Heap *heap = vm_.get_heap(VM::HEAP_CODE);
      Uint8 *tmp_buffer = heap->allocate<Uint8>(size);
      f.seek(0);
      f.read(tmp_buffer, size);

      Std::fmt("Loading BEAM %s\n", path.c_str());
      load_module(proc, name, ArrayView<const Uint8>(tmp_buffer, size));
      heap->deallocate(tmp_buffer);
      return;
    }
  }
  throw err::CodeServer("module not found");
}

Module *Server::find_module(Process *proc, Term m, FindModule load)
{
  auto result = modules_.find_ref(m, nullptr);
  if (!result) {
    if (load == code::FindModule::FindExisting) {
      throw err::CodeServer("function not found");
    } else {
      load_module(proc, m);
      return modules_[m];
    }
  }
  return result;
}

void Server::path_append(const Str &p)
{
  search_path_.push_back(p);
}

void Server::path_prepend(const Str &p)
{
  search_path_.push_front(p);
}

bool Server::print_mfa(Word *ptr) const {
  auto mfa = find_mfa_from_code(ptr);
  if (mfa.mod.is_non_value()) {
    Std::fmt(FMT_0xHEX, (Word)ptr);
    return false;
  }
  Std::fmt("%s:%s/" FMT_UWORD,
         mfa.mod.atom_c_str(vm_), mfa.fun.atom_c_str(vm_), mfa.arity);
  return true;
}

MFArity Server::find_mfa_from_code(Word *ptr) const
{
  Module *m = mod_index_.find(ptr);
  if (!m) {
    return MFArity();
  }
  auto fa = m->find_fun_arity(ptr);
  return MFArity(m->get_name(), fa.fun, fa.arity);
}

Export *Server::find_mfa(const MFArity &mfa, Module **out_mod) const
{
  auto result = modules_.find_ref(mfa.mod, nullptr);
  if (!result) {
    return nullptr;
  }
  if (out_mod) {
    *out_mod = result;
  }
  return result->find_export(mfa.as_funarity());
}

} // ns code
} // ns gluon
