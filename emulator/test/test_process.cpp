#include "g_process.h"
/*
#include "g_code_server.h"
#include "g_code.h"
#include "g_module.h"
#include "g_vm.h"
#include "g_error.h"
#include "g_predef_atoms.h"
#include "g_term_helpers.h"
*/
#include <fructose/fructose.h>
#include "bif/g_bif_misc.h"
#pragma clang diagnostic ignored "-Wweak-vtables"

namespace gluon {

struct process_test_t: public fructose::test_base<process_test_t>
{

  /*void test_process_stack(const std::string &tn) {
    ProcessStack s;
    s.push(Term::make_small_u(1)); // y[3]
    s.push_n_nils(3);              // y[0 1 2]
    s.push(Term::make_small_u(2)); // y[-1]
    s.set_y(1, NONVALUE);
    fructose_assert(s.get_y(0).is_nil());
    fructose_assert(s.get_y(1).is_non_value());
    fructose_assert(s.get_y(2).is_nil());
    fructose_assert(s.get_y(3) == Term::make_small_u(1));
  }*/

}; // struct

void process_test(int argc, const char *argv[]) {
  //process_test_t tests;
  //tests.add_test("process_stack", &process_test_t::test_process_stack);
  //tests.run(argc, const_cast<char **>(argv));
}

} // ns gluon
