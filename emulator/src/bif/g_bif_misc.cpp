#include "bif/g_bif_misc.h"

namespace gluon {
namespace bif {

bool is_term_smaller(Term a, Term b)
{
  if (a == b) {
    return false;
  }

  if (Term::are_both_immed(a, b)) {
    if (Term::are_both_small(a, b)) {
      return a.small_get_value() < b.small_get_value();
    }

    if (a.is_small()) { // means b is not smallint
      return true;
    }

    if (a.is_nil()) { // means b is not nil
      return false;
    }

    if (b.is_nil()) { // means a is not nil
      return true;
    }

//    if (is_atom(a)) {
//      if (is_int(b)) {
//        return 0;
//      } else if (is_atom(b)) {
//        uint8_t *print1 = atoms_get(atom_index(a));
//        uint8_t *print2 = atoms_get(atom_index(b));
//        int short_len = (print1[0] < print2[0])
//                        ? print1[0]
//                        : print2[0];
//        int d = memcmp(print1 + 1, print2 + 1, short_len);

//        if (d == 0) {
//          return print1[0] < print2[0];
//        }

//        return d < 0;
//      } else {
//        return 1;
//      }
//    } else if (is_short_oid(a)) {
//      if (is_int(b) || is_atom(b)) {
//        return 0;
//      } else if (is_short_oid(b)) {
//        return short_oid_id(a) < short_oid_id(b);
//      } else {
//        return 1;
//      }
//    } else if (is_short_pid(a)) {
//      if (is_int(b) || is_atom(b) || is_short_oid(b)) {
//        return 0;
//      } else {
//        assert(is_short_pid(b));
//        return short_pid_id(a) < short_pid_id(b);
//      }
//    }
  }

  G_TODO("compare");
  //return false;
}

} // ns bif
} // ns gluon
