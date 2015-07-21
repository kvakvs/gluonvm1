#include "bif/g_bif_misc.h"
#include "g_process.h"
#include "g_vm.h"

namespace gluon {
namespace bif {

bool is_term_smaller(Term a, Term b)
{
  if (a == b) {
    return false;
  }

  // number < atom < reference < fun < oid < pid < tuple < empty_list < list < binary
  if (Term::are_both_immed(a, b)) {
    if (Term::are_both_small(a, b)) {
      return a.small_get_value() < b.small_get_value();
    }
#if FEATURE_BIGNUM
    // TODO: case when one small and one big? Compare both +-- as big
    //if (Term::are_both_big(a, b)) {
#     error "bigint compare"
    //}
#endif

    if (a.is_small()) { // means b is not smallint
      return true;
    }

    if (a.is_nil()) { // means b is not nil
      return false;
    }

    if (b.is_nil()) { // means a is not nil
      return true;
    }

    if (a.is_atom()) {
      if (b.is_integer()) {
        // Atom > Int
        return false;
      } else if (b.is_atom()) {
        // Compare atoms
        const Str &print1 = VM::find_atom(a);
        const Str &print2 = VM::find_atom(b);
        return print1 < print2;
      } else {
        // Atom is < everything else
        return true;
      }
    }

    if (a.is_port()) {
      // Port is greater than any int or atom
      if (b.is_integer() || b.is_atom()) {
        return false;
      } else if (b.is_short_port()) {
        return a.short_port_get_value() < b.short_port_get_value();
      } else {
        return true;
      }
    } else if (a.is_pid()) {
      if (b.is_integer() || b.is_atom() || b.is_port()) {
        return 0;
      } else {
#if FEATURE_ERL_DIST
#       error "long pids?"
#endif
        G_ASSERT(b.is_short_pid());
        return a.short_pid_get_value() < b.short_pid_get_value();
      }
    }
  }

  G_TODO("compare");
  //return false;
}

Term bif_minus_2(Process *, Term a, Term b)
{
  G_ASSERT(a.is_small());
  G_ASSERT(b.is_small());
  sword_t a_s = a.small_get_value();
  sword_t b_s = b.small_get_value();
  printf("bif minus %zd - %zd\n", a_s, b_s);
  return Term::make_small(a_s - b_s);
}

Term bif_length_1(Process *, Term a)
{
  if (a.is_nil()) {
    return Term::make_small(0);
  }

  G_ASSERT(a.is_cons());
  sword_t counter = 1;
  while (a.is_cons()) {
    a = a.cons_get_element(1);
    counter++;
  }
  if (!a.is_nil()) {
    G_FAIL("throw badarg");
  }
  return Term::make_small(counter);
}

bool are_terms_equal(Term a, Term b, bool exact)
{
  G_ASSERT(a != b); // should be checked elsewhere


}


} // ns bif
} // ns gluon
