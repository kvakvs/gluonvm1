#include "g_vm.h"
#include "g_codeserver.h"

namespace gluon {

str_atom_map_t VM::g_atoms;
atom_str_map_t VM::g_atoms_reverse;

void VM::init()
{
  CodeServer::init();
}

} // ns gluon
