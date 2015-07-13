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
  static void load_module(Term name);
};

} // ns gluon
