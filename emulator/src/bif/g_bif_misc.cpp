#include "bif/g_bif_misc.h"
#include "g_process.h"
#include "g_predef_atoms.h"
#include "g_vm.h"
#include "g_heap.h"
#include "g_module.h"
#include "g_code_server.h"
#include "g_term_helpers.h"

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
      return a.small_get_signed() < b.small_get_signed();
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

Term bif_less_equal_2(Process *, Term a, Term b)
{
  if (!is_term_smaller(b, a)) {
    return atom::TRUE;
  }
  return atom::FALSE;
}

Term bif_greater_equal_2(Process *, Term a, Term b)
{
  if (!is_term_smaller(a, b)) {
    return atom::TRUE;
  }
  return atom::FALSE;
}

Term bif_atom_to_list_1(Process *proc, Term a)
{
  if (!a.is_atom()) {
    return proc->bif_error(atom::BADARG);
  }

  const Str &atom_str = VM::find_atom(a);
//  return term::build_string(proc->get_heap(), atom_str);
  auto x = term::build_string(proc->get_heap(), atom_str);
  x.println();
  return x;
}

Term bif_minus_2(Process *proc, Term a, Term b)
{
  if (!a.is_small() || !b.is_small()) {
    return proc->bif_error(atom::BADARITH);
  }
  sword_t a_s = a.small_get_signed();
  sword_t b_s = b.small_get_signed();
  return Term::make_small(a_s - b_s);
}

Term bif_plus_2(Process *proc, Term a, Term b)
{
  if (!a.is_small() || !b.is_small()) {
    return proc->bif_error(atom::BADARITH);
  }
  sword_t a_s = a.small_get_signed();
  sword_t b_s = b.small_get_signed();
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
Term bif_multiply_2(Process *proc, Term a, Term b)
{
  if (!a.is_small() || !b.is_small()) {
    return proc->bif_error(atom::BADARITH);
  }
  sword_t a_s = a.small_get_signed();
  sword_t b_s = b.small_get_signed();
  return Term::make_small(a_s * b_s);
}

Term bif_divide_2(Process *proc, Term a, Term b)
{
  if (!a.is_small() || !b.is_small()) {
    return proc->bif_error(atom::BADARITH);
  }
  sword_t a_s = a.small_get_signed();
  sword_t b_s = b.small_get_signed();
  return Term::make_small(a_s / b_s);
}

// Create an export value
Term bif_make_fun_3(Process *proc, Term mod, Term f, Term arity_t)
{
  word_t arity = arity_t.small_get_unsigned();

  // Box export (1 word for boxed tag and 1 word reference to export_t)
  // TODO: calculate is_bif for new object
  mfarity_t mfa(mod, f, arity);
  void *biffn = VM::find_bif(mfa);
  export_t *box = Heap::alloc_object<export_t>(proc->get_heap(),
                                               biffn != nullptr);
  box->mfa = mfa;
  if (biffn == nullptr) {
    // Find module
    auto m_result = VM::get_cs()->find_module(mod, code::LOAD_IF_NOT_FOUND);
    if (m_result.is_error()) {
      return proc->bif_error(atom::UNDEF);
    }
    // Find export in module
    Module *m = m_result.get_result();
    word_t *code = m->find_export(fun_arity_t(f, arity));
    box->code = code;
  } else {
    box->bif_fn = biffn;
  }
  return Term::make_boxed_export(box);
}

static Term integer_to_list(Process *proc, Term n, word_t base)
{
  if (base < 2 || base > 36) {
    return proc->bif_error(atom::BADARG);
  }

  if (n.is_small()) {
    sword_t v = n.small_get_signed();

    char buf[16];
    char *ptr = buf + sizeof(buf) - 1;
    const char *endptr = ptr;
    // We do not need trailing zero as we use end pointer to delimit string

    bool is_neg = v < 0;

    if (v < 0) {
      v = -v;
    }

    do {
      word_t d = (word_t)v % base;
      v /= 10;
      *ptr-- = '0' + (char)d;
    } while (v > 0);

    if (is_neg) {
      *ptr = '-';
    } else {
      ptr++;
    }

    return term::build_list(proc->get_heap(),
                            const_cast<const char *>(ptr), endptr);
  }
#if FEATURE_BIGNUM
  else if (is_boxed(N) && is_bignum(peel_boxed(N))) {
    //TODO: unbounded alloc: burn fat
    bignum_t *bn = (bignum_t *)peel_boxed(N);
    int n = bignum_str_size(bn);
    char buf[n];
    bignum_to_str(&proc->hp, bn, buf);
    return heap_strz(&proc->hp, buf);
  }
#endif

  return proc->bif_error(atom::BADARG);
}

Term bif_integer_to_list_1(Process *proc, Term n)
{
  return integer_to_list(proc, n, 10);
}

Term bif_integer_to_list_2(Process *proc, Term n, Term base)
{
  return integer_to_list(proc, n, base.small_get_unsigned());
}

} // ns bif
} // ns gluon
