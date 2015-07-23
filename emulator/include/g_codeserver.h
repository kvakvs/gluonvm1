#pragma once

#include "g_defs.h"
#include "g_term.h"
#include "g_error.h"
#include "g_code.h"

//#include <stdint.h>

namespace gluon {


// Code pointer, refers to module name, version and offset
//typedef struct {
//  Term    name;
//  word_t  version;
//  word_t  offset;
//} code_ptr_t;

class Module;

using mod_map_t = Map<Term, Module *>;

// Note: singleton, do not instantiate even
class CodeServer {
private:
  CodeServer() = delete;
  static mod_map_t g_modules;
  static List<Str> g_search_path;

public:
  static void init();
  static MaybeError load_module(Term name_atom);
  // Pass nil as name to take name automatically from the module
  static MaybeError load_module(Term name_atom, const u8_t *bytes, word_t size);

  typedef enum {
    FIND_EXISTING,
    LOAD_IF_NOT_FOUND
  } find_opt_t;
  static Result<Module *> find_module(Term m, find_opt_t opt);
  static void path_append(const Str &p);
  static void path_prepend(const Str &p);

protected:
  static Result<Module *> load_module_internal(Term expected_name_or_nil,
                                               const u8_t *bytes, word_t size);
};

} // ns gluon
