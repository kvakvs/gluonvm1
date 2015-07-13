#pragma once

#include "g_defs.h"
#include "g_term.h"

#include <stdint.h>

namespace gluon {

class Module {
public:
};

using mod_map_t = Map<Str, Module *>;

// Note: singleton, do not instantiate even
class CodeServer {
private:
  CodeServer() = delete;
  static mod_map_t g_modules;

public:
  static void init();
  // Pass nil as name to take name automatically from the module
  static void load_module(Term name_atom, const u8_t *bytes, word_t size);
};

} // ns gluon
