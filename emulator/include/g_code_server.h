#pragma once

#include "g_defs.h"
#include "g_term.h"
#include "g_error.h"
#include "g_code.h"
#include "g_code_index.h"

//#include <stdint.h>

namespace gluon {


// Code pointer, refers to module name, version and offset
//typedef struct {
//  Term    name;
//  word_t  version;
//  word_t  offset;
//} code_ptr_t;

class Module;

namespace code {

using mod_map_t = Map<Term, Module *>;

class Server {
private:
  mod_map_t   m_modules;
  List<Str>   m_search_path;

#if FEATURE_CODE_RANGES
  // Map code range to Module*
  code::Index<Module *> m_mod_index;
#endif

public:
  Server() {}
//  void init();
  MaybeError load_module(Term name_atom);
  // Pass nil as name to take name automatically from the module
  MaybeError load_module(Term name_atom, const u8_t *bytes, word_t size);

  typedef enum {
    FIND_EXISTING,
    LOAD_IF_NOT_FOUND
  } find_opt_t;
  Result<Module *> find_module(Term m, find_opt_t opt);
  void path_append(const Str &p);
  void path_prepend(const Str &p);

  // Find module, function and arity for code location and print it
  void print_mfa(word_t *ptr) const;
  mfarity_t find_mfa(word_t *ptr) const;

protected:
  Result<Module *> load_module_internal(Term expected_name_or_nil,
                                        const u8_t *bytes, word_t size);
};

} // ns code
} // ns gluon
