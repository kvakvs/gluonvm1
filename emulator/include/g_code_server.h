#pragma once

#include "g_defs.h"
#include "g_term.h"
#include "g_error.h"
#include "g_code.h"
#include "g_code_index.h"

namespace gluon {


// Code pointer, refers to module name, version and offset
//typedef struct {
//  Term    name;
//  word_t  version;
//  word_t  offset;
//} code_ptr_t;

class Module;
class VM;

namespace code {

using mod_map_t = Dict<Term, Module *>;

typedef enum {
  FIND_EXISTING,
  LOAD_IF_NOT_FOUND
} find_opt_t;

class Server {
private:
  VM          &vm_;
  mod_map_t   modules_;
  List<Str>   search_path_;

#if FEATURE_CODE_RANGES
  // Map code range to Module*
  code::Index<Module *> mod_index_;
#endif

public:
  Server(VM &v): vm_(v) {}
//  void init();
  void load_module(Process *proc, Term name_atom);
  // Pass nil as name to take name automatically from the module
  void load_module(Process *proc, Term name_atom,
                   const u8_t *bytes, word_t size);

  Module *find_module(Process *proc, Term m, find_opt_t opt);
  void path_append(const Str &p);
  void path_prepend(const Str &p);

  // Find module, function and arity for code location and print it.
  // Returns true if mfa was found and printed, else false
  bool print_mfa(word_t *ptr) const;
  mfarity_t find_mfa_from_code(word_t *ptr) const;
  export_t *find_mfa(const mfarity_t &mfa, Module **out_mod=nullptr) const;

protected:
  Module *load_module_internal(proc::Heap *heap,
                               Term expected_name_or_nil,
                               const u8_t *bytes, word_t size);
};

} // ns code
} // ns gluon
