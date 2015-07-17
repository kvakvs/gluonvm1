#include "g_vm.h"
#include "g_process.h"

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
  return 0;
#else

  // normal start
  vm::load_module("../test/g_test1.S.gleam");
  gluon::Process p;
  p.jump(vm::to_atom("g_test1"), vm::to_atom("test1"), gluon::Term::make_nil());

  return 0;
#endif
}
