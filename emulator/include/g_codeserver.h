#pragma once

#include "g_defs.h"
#include "g_term.h"
#include "g_error.h"

#include <stdint.h>

namespace gluon {

using code_t = UniquePtr<u8_t>;

// Code pointer, refers to module name, version and offset
typedef struct {
  Term    name;
  word_t  version;
  word_t  offset;
} code_ptr_t;

class Module {
public:
  Term    m_name;
  code_t  m_code;
};

using mod_map_t = Map<Term, Module *>;

// Note: singleton, do not instantiate even
class CodeServer {
private:
  CodeServer() = delete;
  static mod_map_t g_modules;

public:
  static void init();
  // Pass nil as name to take name automatically from the module
  static MaybeError load_module(Term name_atom, const u8_t *bytes, word_t size);

protected:
  static Result<Module *> load_module_internal(Term name_atom,
                                               const u8_t *bytes, word_t size);
};

} // ns gluon
