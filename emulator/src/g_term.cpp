#include "g_term.h"
#include "g_vm.h"
#include "g_heap.h"

#if G_TEST
#include <fructose/fructose.h>
#endif

namespace gluon {

word_t term::g_zero_sized_tuple = 0;

#if FEATURE_MAPS
word_t term::g_zero_sized_map = term_tag::BoxedMap::create_subtag(0);
#endif

Term Term::allocate_cons(Heap *heap, Term head, Term tail) {
  Term *d = Heap::alloc<Term>(heap, 2);
  d[0] = head;
  d[1] = tail;
  return make_cons(d);
}

Str Term::atom_str() const
{
  return VM::find_atom(*this);
}

#if G_DEBUG
void Term::print()
{
  if (m_val == 0) {
    printf("BADTERM");
    return;
  }
  if (is_cons()) {
    printf("[");
    cons_head().print();
    printf(",");
    cons_tail().print();
    printf("]");
  }
  else if (is_tuple()) {
    auto arity = tuple_get_arity();
    printf("{");
    for (word_t n = 0; n < arity; ++n) {
      tuple_get_element(n).print();
      if (n < arity-1) {
        printf(",");
      }
    }
    printf("};");
  }
  else if (is_boxed()) {
    auto p = boxed_get_ptr<word_t>();
    if (term_tag::is_cp<word_t>(p)) {
      printf("BOXED_CP(0x%zx)", (word_t)term_tag::untag_cp<word_t>(p));
    } else {
      printf("BOXED(0x%zx)", boxed_get_subtag());
    }
  }
  else if (is_nil()) {
    printf("NIL");
  }
  else if (is_non_value()) {
    printf("NON_VALUE");
  }
  else if (is_atom()) {
    printf("'%s'", atom_str().c_str());
  }
  else if (is_small()) {
    printf("%zd", small_get_value());
  }
  else if (is_short_pid()) {
    printf("PID");
  }
  else if (is_regx()) {
    printf("X[%zu]", regx_get_value());
  }
#if FEATURE_FLOAT
  else if (is_regfp()) {
    printf("FP[%zu]", regfp_get_value());
  }
#endif
  else if (is_regy()) {
    printf("Y[%zu]", regy_get_value());
  }
  else {
    printf("UNKNOWN(%zx)", m_val);
  }
}

void Term::println()
{
  print();
  puts("");
}
#endif // DEBUG

//
//====================================
//
#if G_TEST
} // ns gluon

#include "bif/g_bif_misc.h"
#pragma clang diagnostic ignored "-Wweak-vtables"
namespace gluon {
struct term_test_t: public fructose::test_base<term_test_t>
{

  void test_term_basics(const std::string& test_name) {
    Term t_tuple_el[10];
    Term t_tuple = Term::make_tuple(t_tuple_el, 10);
    fructose_assert(t_tuple.is_tuple());
  }
  void test_term_cmp(const std::string &tn) {
    Term l1 = Term::allocate_cons(nullptr, Term::make_small(3), Term::make_nil());
    Term l2 = Term::allocate_cons(nullptr, Term::make_small(2), l1);
    Term l3 = Term::allocate_cons(nullptr, Term::make_small(1), l2);

    Term m1 = Term::allocate_cons(nullptr, Term::make_small(1), Term::make_nil());
    Term m2 = Term::allocate_cons(nullptr, Term::make_small(2), m1);
    Term m3 = Term::allocate_cons(nullptr, Term::make_small(3), m2);

    fructose_assert(bif::are_terms_equal(l3, m3, false) == false);
  }

}; // struct

void term_test(int argc, const char *argv[]) {
  term_test_t tests;
  tests.add_test("term_basics", &term_test_t::test_term_basics);
  tests.add_test("term_cmp", &term_test_t::test_term_cmp);
  tests.run(argc, const_cast<char **>(argv));
}

#endif // TEST


} // ns gluon
