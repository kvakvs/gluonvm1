#include "test.h"
#include "g_sys_stdlib.h"

using namespace gluon;
namespace gluontest {

void run_tests(int argc, const char *argv[]) {
  gluon_test_terms(argc, argv);
  gluon_test_processes(argc, argv);
  gluon_test_ranges(argc, argv);

  //Std::exit(0);
}

} // ns gluontest
