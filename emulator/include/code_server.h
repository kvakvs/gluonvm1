#pragma once

#include "defs.h"
#include "term.h"
#include "error.h"
#include "code.h"
#include "code_index.h"
#include "struct/g_array.h"

namespace gluon {

// Code pointer, refers to module name, version and offset
// typedef struct {
//  Term    name;
//  Word  version;
//  Word  offset;
//} code_ptr_t;

class Module;
class VM;

namespace code {

using ModuleMap = Dict<Term, Module*>;

enum class FindModule { FindExisting, LoadIfNotFound };

class Server {
 private:
  VM& vm_;
  ModuleMap modules_;
  List<Str> search_path_;

#if FEATURE_CODE_RANGES
  // Map code range to Module*
  code::Index<Module*> mod_index_;
#endif

 public:
  Server(VM& v) : vm_(v) {}
  //  void init();
  void load_module(Process* proc, Term name_atom);

  // Pass nil as name to take name automatically from the module
  void load_module(Process* proc, Term name_atom, ArrayView<const Uint8> data);

  Module* find_module(Process* proc, Term m, FindModule opt);
  void path_append(const Str& p);
  void path_prepend(const Str& p);

  // Find module, function and arity for code location and print it.
  // Returns true if mfa was found and printed, else false
  bool print_mfa(const Word* ptr) const;
  bool find_mfa_from_code(const Word* ptr, MFArity& out) const;
  Export* find_mfa(const MFArity& mfa, Module** out_mod = nullptr) const;

 protected:
  Module* load_module_internal(proc::Heap* heap,
                               Term expected_name_or_nil,
                               ArrayView<const Uint8> data);
};

}  // ns code
}  // ns gluon
