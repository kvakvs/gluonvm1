#include "g_vm.h"

using vm = gluon::VM;

#if G_TEST
void run_tests(int argc, const char *argv[]) {
  gluon::term_test(argc, argv);
}
#endif

int main(int argc, const char *argv[]) {

  vm::init();

#if G_TEST
  // test runner
  run_tests(argc, argv);
#else
  // normal start
  vm::load_module("../test/g_test1.S.gleam");
#endif

  return 0;
}
