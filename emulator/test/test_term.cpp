#include "g_term.h"
/*
#include "g_vm.h"
#include "g_heap.h"
#include "g_code_server.h"
#include "g_fun.h"
#include "g_module.h" // for export_t class
#include "g_term_helpers.h"
#include "g_heap.h"
#include "g_binary.h"
*/

#include <fructose/fructose.h>
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
    Term l1 = Term::allocate_cons(nullptr, Term::make_small(3), NIL);
    Term l2 = Term::allocate_cons(nullptr, Term::make_small(2), l1);
    Term l3 = Term::allocate_cons(nullptr, Term::make_small(1), l2);

    Term m1 = Term::allocate_cons(nullptr, Term::make_small(1), NIL);
    Term m2 = Term::allocate_cons(nullptr, Term::make_small(2), m1);
    Term m3 = Term::allocate_cons(nullptr, Term::make_small(3), m2);

    fructose_assert(bif::are_terms_equal(l3, m3, false) == false);
  }
  void test_term_small(const std::string& test_name) {
    Term s1 = Term::make_small(-1);
    Term s2 = Term::make_small(0);
    Term s3 = Term::make_small(1);
    fructose_assert(s1.small_get_signed() == -1);
    fructose_assert(s2.small_get_signed() == 0);
    fructose_assert(s3.small_get_signed() == 1);
  }
}; // struct

void term_test(int argc, const char *argv[]) {
  term_test_t tests;
  tests.add_test("term_basics", &term_test_t::test_term_basics);
  tests.add_test("term_cmp", &term_test_t::test_term_cmp);
  tests.add_test("term_small", &term_test_t::test_term_small);
  tests.run(argc, const_cast<char **>(argv));
}

} // ns gluon
