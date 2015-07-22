#include "bif/g_bif_misc.h"
#include "g_process.h"
#include "g_predef_atoms.h"
#include "g_vm.h"

namespace gluon {
namespace bif {

#if 0
static int term_order(Term t)
{
  // number < atom < reference < fun < oid < pid < tuple < empty_list < list < binary
  enum { O_NUMBER, O_ATOM, O_REF, O_FUN, O_EXPORT, O_PORT, O_PID, O_TUPLE,
         O_MAP, O_NIL, O_CONS, O_BINARY };

  if (t.is_cons()) {
    return O_CONS;
  }

  if (t.is_tuple()) {
    return O_TUPLE;
  }

  if (t.is_nil()) {
    return O_NIL;
  }

  if (t.is_integer()) {
    return O_NUMBER;
  }

  if (t.is_atom()) {
    return O_ATOM;
  }

  if (t.is_short_pid()) {
    return O_PID;
  }

  if (t.is_short_port()) {
    return O_PORT;
  }

  G_ASSERT(t.is_boxed());

  word_t subtag = t.boxed_get_subtag();
  switch (subtag) {
  case term_tag::BOXED_POS_BIGNUM:
  case term_tag::BOXED_NEG_BIGNUM:
#if FEATURE_FLOAT
  case term_tag::BOXED_FLOAT:
#endif
    return O_NUMBER;

  case term_tag::BOXED_FUN:
    return O_FUN;

  case term_tag::BOXED_EXPORT:
    return O_EXPORT;

  case term_tag::BOXED_MAP:
    return O_MAP;

  case term_tag::BOXED_PID:
    return O_PID;

  case term_tag::BOXED_PORT:
    return O_PORT;

  case term_tag::BOXED_REF:
    return O_REF;

  case term_tag::BOXED_PROC_BIN:
  case term_tag::BOXED_HEAP_BIN:
  case term_tag::BOXED_MATCH_CTX:
  case term_tag::BOXED_SUB_BIN:
    return O_BINARY;

  default:
    // crash vm here :(
    G_FAIL("what term is it?");
  }
}
#endif // 0

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

  G_TODO("compare boxed values");
  //return false;

  // a and b are quaranteed to have different types
  G_IF_NODEBUG(return term_order(a) < term_order(b));
}

Term bif_minus_2(Process *, Term a, Term b)
{
  G_ASSERT(a.is_small());
  G_ASSERT(b.is_small());
  sword_t a_s = a.small_get_value();
  sword_t b_s = b.small_get_value();
  return Term::make_small(a_s - b_s);
}

Term bif_plus_2(Process *, Term a, Term b)
{
  G_ASSERT(a.is_small());
  G_ASSERT(b.is_small());
  sword_t a_s = a.small_get_value();
  sword_t b_s = b.small_get_value();
  return Term::make_small(a_s + b_s);
}


Term bif_length_1(Process *, Term a)
{
  if (a.is_nil()) {
    return Term::make_small(0);
  }

  printf("length: "); a.println();
  G_ASSERT(a.is_cons());
  sword_t counter = 0;
  while (a.is_cons()) {
    a = a.cons_tail();
    counter++;
  }
  if (!a.is_nil()) {
    G_FAIL("throw badarg");
  }
  return Term::make_small(counter);
}

bool are_terms_equal(Term a, Term b, bool exact)
{
  printf("are_terms_eq(exact=%d): ", (int)exact);
  a.print();
  printf(" ? ");
  b.println();
  if (a == b) {
    return true;
  }

  if (a.is_immed() || b.is_immed()) {
    if (exact) {
      // We assume binary term equality was checked before calling this function
      // so immediate value can't be exactly equal to anything else
      return false;
    }

    if (a.is_integer() && b.is_boxed()) {
#if FEATURE_FLOAT
      word_t *term_data = peel_boxed(b);
      return (boxed_tag(term_data) == SUBTAG_FLOAT)
             && (double)int_value(a) == float_value(term_data);
#else
      return false;
#endif
    } else if (a.is_boxed() && b.is_integer()) {
#if FEATURE_FLOAT
      uint32_t *term_data = peel_boxed(a);
      return (boxed_tag(term_data) == SUBTAG_FLOAT)
             && (float_value(term_data) == (double)int_value(b));
#else
      return false;
#endif
    }
    return false;
//#endif
  }

  if (a.is_cons()) {
    if (b.is_cons()) {
      printf("both cons\n");
      do {
        Term *cons_a = a.boxed_get_ptr<Term>();
        Term *cons_b = b.boxed_get_ptr<Term>();

        if (cons_a[0] != cons_b[0]
            && !are_terms_equal(cons_a[0], cons_b[0], exact)) {
          return false;
        }

        a = cons_a[1];
        b = cons_b[1];
      } while (a.is_cons() && b.is_cons());

      return (a == b) || are_terms_equal(a, b, exact);
    } else {
      return false;
    }
  } else if (a.is_tuple()) {
    if (b.is_tuple()) {
      Term *a_ptr = a.boxed_get_ptr<Term>();
      Term *b_ptr = a.boxed_get_ptr<Term>();
      if (a_ptr[0] != b_ptr[0]) {
        return false;
      }

      for (word_t i = 1; i <= ((word_t *)a_ptr)[0]; i++)
        if (a_ptr[i] != b_ptr[i]
            && !are_terms_equal(a_ptr[i], b_ptr[i], exact)) {
          return false;
        }

      return true;
    } else {
      return false;
    }
  } else {
    G_ASSERT(a.is_boxed());

    if (!b.is_boxed()) {
      return false;
    }

    G_TODO("compare boxed");
    /*
    uint32_t *term_data1 = peel_boxed(a);
    uint32_t *term_data2 = peel_boxed(b);

    uint32_t subtag = boxed_tag(term_data1);

    if (!exact && subtag == SUBTAG_FLOAT && is_bignum(term_data2)) {
      return float_value(term_data1) == bignum_to_double((bignum_t *)term_data2);
    }

    if (!exact && is_bignum(term_data1) && boxed_tag(term_data2) == SUBTAG_FLOAT) {
      return bignum_to_double((bignum_t *)term_data1) == float_value(term_data2);
    }

    if (subtag != boxed_tag(term_data2) &&
        !(is_binary(term_data1) && is_binary(term_data2))) {
      return 0;
    }

    switch (subtag) {
    case SUBTAG_POS_BIGNUM:
    case SUBTAG_NEG_BIGNUM: {
      bignum_t *bn1 = (bignum_t *)term_data1;
      bignum_t *bn2 = (bignum_t *)term_data2;
      return bignum_compare(bn1, bn2) == 0;
    }

    case SUBTAG_FUN: {
      t_fun_t *f1 = (t_fun_t *)term_data1;
      t_fun_t *f2 = (t_fun_t *)term_data2;

      if (f1->module != f2->module ||
          f1->index != f2->index ||
          f1->old_uniq != f2->old_uniq) {
        return 0;
      }

      int num_free = fun_num_free(term_data1);
      assert(num_free == fun_num_free(term_data2));

      for (int i = 0; i < num_free; i++) {
        term_t v1 = f1->frozen[i];
        term_t v2 = f2->frozen[i];

        if (v1 != v2 && !are_terms_equal(v1, v2, exact)) {
          return 0;
        }
      }

      return 1;
    }

    case SUBTAG_EXPORT: {
      export_t *e1 = ((t_export_t *)term_data1)->e;
      export_t *e2 = ((t_export_t *)term_data2)->e;
      return e1->module == e2->module &&
             e1->function == e2->function &&
             e1->arity == e2->arity;
    }

    case SUBTAG_MAP: {
      t_map_t *m1 = (t_map_t *)term_data1;
      t_map_t *m2 = (t_map_t *)term_data2;
      int size = map_size(m1);

      if (size != map_size(m2)) {
        return 0;
      }

      if (!are_terms_equal(m1->keys, m2->keys, exact)) {
        return 0;
      }

      for (int i = 0; i < size; i++)
        if (!are_terms_equal(m1->values[i], m2->values[i], exact)) {
          return 0;
        }

      return 1;
    }

    case SUBTAG_PID: {
      t_long_pid_t *pid1 = (t_long_pid_t *)term_data1;
      t_long_pid_t *pid2 = (t_long_pid_t *)term_data2;
      return pid1->node == pid2->node &&
             pid1->serial == pid2->serial &&
             opr_hdr_id(pid1) == opr_hdr_id(pid2) &&
             opr_hdr_creat(pid1) == opr_hdr_creat(pid2);
    }

    case SUBTAG_OID: {
      t_long_oid_t *oid1 = (t_long_oid_t *)term_data1;
      t_long_oid_t *oid2 = (t_long_oid_t *)term_data2;
      return oid1->node == oid2->node &&
             opr_hdr_id(oid1) == opr_hdr_id(oid2) &&
             opr_hdr_creat(oid1) == opr_hdr_creat(oid2);
    }

    case SUBTAG_REF: {
      t_long_ref_t *ref1 = (t_long_ref_t *)term_data1;
      t_long_ref_t *ref2 = (t_long_ref_t *)term_data2;
      return ref1->node == ref2->node &&
             ref1->id1 == ref2->id1 &&
             ref1->id2 == ref2->id2 &&
             opr_hdr_id(ref1) == opr_hdr_id(ref2) &&
             opr_hdr_creat(ref1) == opr_hdr_creat(ref2);
    }

    case SUBTAG_PROC_BIN:
    case SUBTAG_HEAP_BIN:
    case SUBTAG_MATCH_CTX:
    case SUBTAG_SUB_BIN: {
      bits_t bs1, bs2;
      bits_get_real(term_data1, &bs1);
      bits_get_real(term_data2, &bs2);
      return (bits_compare(&bs1, &bs2) == 0);
    }

    default:
      assert(subtag == SUBTAG_FLOAT);
      return float_value(term_data1) == float_value(term_data2);
    }

    return 1;
    */

  }

}

Term bif_equals_2(Process *, Term a, Term b)
{
  if (are_terms_equal(a, b, false)) {
    return atom::TRUE;
  }
  return atom::FALSE;
}

Term bif_equals_exact_2(Process *, Term a, Term b)
{
  if (are_terms_equal(a, b, true)) {
    return atom::TRUE;
  }
  return atom::FALSE;
}

} // ns bif
} // ns gluon
