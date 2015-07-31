#!/usr/bin/python
# takes: genop.tab from erlang/otp
# produces: sorted bif search table
import libgenop

libgenop.load()

print("""// Generated by codegen/vm_bif_tab_*.py
#include "g_vm_bif_tab.h"
#include "g_predef_atoms.h"
#include "bif/g_bif_misc.h"

namespace gluon {
namespace bif {

const bif_index_t g_bif_table[BIF_TABLE_SIZE] = {""")

# print atom constants
for b in libgenop.bif_tab:
    arity = int(b['arity'])
    bif_atom_id = libgenop.atom_id_tab[b['atom']]
    bif_atom = libgenop.id_atom_tab[bif_atom_id]
    bifname = libgenop.atom_constname(bif_atom)
    print('  {atom::%s, %d, (void *)&bif_%s_%d}, // atom id=%d' \
        % (bifname, arity, b['cname'], arity, bif_atom_id))

print("""};

} // ns bif
} // ns gluon
""")
