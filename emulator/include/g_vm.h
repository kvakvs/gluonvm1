#pragma once

#include "g_defs.h"
#include "g_term.h"

namespace gluon {

using str_atom_map_t = Map<Str, Term>;
using atom_str_map_t = Map<Term, Str>;

// Note: singleton, do not instantiate even
class VM {
private:
  VM() = delete;
  static str_atom_map_t g_atoms;
  static atom_str_map_t g_atoms_reverse;

public:
  static void init();
};

} // ns gluon
