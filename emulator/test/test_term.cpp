#include "test.h"
#include "g_heap.h"
#include "g_term.h"
#include "g_vm.h"

/*
#include "g_binary.h"
#include "g_code_server.h"
#include "g_fun.h"
#include "g_heap.h"
#include "g_module.h" // for export_t class
#include "g_term_helpers.h"
*/

#include "../test/fructose/fructose.h"
#include "bif/g_bif_misc.h"

#pragma clang diagnostic ignored "-Wweak-vtables"

using namespace gluon;
namespace gluontest {

struct term_test_t : public fructose::test_base<term_test_t> {
    void basics(const std::string& test_name) {
        Term t_tuple_el[10];
        Term t_tuple = Term::make_tuple(t_tuple_el, 10);
        fructose_assert(t_tuple.is_tuple());
    }

    void comparison(const std::string& tn) {
        proc::Heap heap;
        Term l1 = Term::allocate_cons(&heap, Term::make_small(3), NIL);
        Term l2 = Term::allocate_cons(&heap, Term::make_small(2), l1);
        Term l3 = Term::allocate_cons(&heap, Term::make_small(1), l2);

        Term m1 = Term::allocate_cons(&heap, Term::make_small(1), NIL);
        Term m2 = Term::allocate_cons(&heap, Term::make_small(2), m1);
        Term m3 = Term::allocate_cons(&heap, Term::make_small(3), m2);

        fructose_assert(bif::are_terms_equal(l3, m3, false) == false);
    }

    void small(const std::string& test_name) {
        Term s1 = Term::make_small(-1);
        Term s2 = Term::make_small(0);
        Term s3 = Term::make_small(1);
        fructose_assert(s1.small_get_signed() == -1);
        fructose_assert(s2.small_get_signed() == 0);
        fructose_assert(s3.small_get_signed() == 1);
    }

    void proc_bin(const std::string& tn) {
        proc::Heap heap;
        Term pb1 = Term::make_binary(&heap, 1024);
        u8_t* ptr1 = pb1.binary_get<u8_t>();
        // There was a moment when ptr returned was a very small integer.
        // TODO: Fix this test to make more sense
        fructose_assert((word_t)ptr1 > 0x10000);
    }
};  // struct

void gluon_test_terms(int argc, const char* argv[]) {
    term_test_t tests;
    tests.add_test("term.basics", &term_test_t::basics);
    tests.add_test("term.comparison", &term_test_t::comparison);
    tests.add_test("term.small", &term_test_t::small);
    tests.add_test("term.procbin", &term_test_t::proc_bin);
    tests.run(argc, const_cast<char**>(argv));
}

}  // ns gluontest
