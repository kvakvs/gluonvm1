#include "code_server.h"
#include "module.h"
#include "platf/gsys_file.h"
#include "heap.h"
#include "vm.h"
#include "process.h"

namespace gluon {
namespace code {

// mod_map_t Server::g_modules;
// List<Str> Server::g_search_path;

// void Server::init() {
//}

void Server::load_module(Process* proc,
                         Term name_atom,
                         ArrayView<const Uint8> data) {
  // TODO: module versions for hot code loading
  // TODO: free if module already existed, check module usage by processes
  Module* m = load_module_internal(proc->get_heap(), name_atom, data);
  modules_.insert(m->get_name(), m);

  // assume that mod already registered own functions in own fun index
  // code to fun/arity mapping should be updated on loading stage
  if (feature_code_ranges) {
    auto range = m->get_code_range();
    mod_index_.add(range, m);
  }
}

void Server::load_module(Process* proc, Term name) {
  // Scan for locations where module file can be found
  Str mod_filename(name.atom_str(vm_));
  mod_filename += ".beam";

  for (const Str& dir : search_path_) {
    Str path(dir);
    path += "/";
    path += mod_filename;

    if (fs::exists(path)) {
      fs::File f;
      f.open(path);

      Word size = f.size();
      erts::Heap* heap = vm_.get_heap(VM::HEAP_CODE);
      Uint8* tmp_buffer = heap->allocate<Uint8>(size);
      f.seek(0);
      f.read(tmp_buffer, size);

      Std::fmt("Loading BEAM %s\n", path.c_str());
      load_module(proc, name, ArrayView<const Uint8>(tmp_buffer, size));
      heap->deallocate_many(tmp_buffer, size);
      return;
    }
  }
  throw err::CodeServer("module not found");
}

Module* Server::find_module(Process* proc, Term m, FindModule load) {
  auto presult = modules_.find_ptr(m);
  if (!presult) {
    if (load == code::FindModule::FindExisting) {
      throw err::CodeServer("function not found");
    } else {
      load_module(proc, m);
      auto mptr = modules_.find_ptr(m);
      return mptr ? *mptr : nullptr;
    }
  }
  return *presult;
}

void Server::path_append(const Str& p) {
  search_path_.push_back(p);
}

void Server::path_prepend(const Str& p) {
  search_path_.push_front(p);
}

bool Server::print_mfa(const Word* ptr) const {
  MFArity mfa;
  if (!find_mfa_from_code(ptr, /*out*/ mfa)) {
    Std::fmt(FMT_0xHEX, (Word)ptr);
    return false;
  }
  if (!mfa.mod.is_atom() || !mfa.fun.is_atom()) {
    throw err::CodeServer("mfa is not atom:atom");
  }
  mfa.mod.print(vm_);
  Std::fmt(":");
  mfa.fun.print(vm_);
  Std::fmt("/" FMT_UWORD, mfa.arity);

  return true;
}

bool Server::find_mfa_from_code(const Word* ptr, MFArity& out) const {
  Module* m = nullptr;
  if (!mod_index_.find(ptr, /*out*/ m) || !m) {
    return false;
  }
  FunArity fa;
  if (!m->find_fun_arity(ptr, /*out*/ fa)) {
    return false;
  }
  out = MFArity(m->get_name(), fa);
  return true;
}

Export* Server::find_mfa(const MFArity& mfa, Module** out_mod) const {
  auto presult = modules_.find_ptr(mfa.mod);
  if (!presult) {
    return nullptr;
  }
  if (out_mod) {
    *out_mod = *presult;
  }
  return (*presult)->find_export(mfa.as_funarity());
}

}  // ns code
}  // ns gluon
