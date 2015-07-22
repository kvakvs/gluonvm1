#include "g_vm.h"
#include "g_process.h"
#include "g_error.h"

#include <stdio.h>

using vm = gluon::VM;

#if G_TEST
static void run_tests(int argc, const char *argv[]) {
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
  gluon::Process proc;
  auto j_result = proc.call(vm::to_atom("g_test1"), vm::to_atom("test"), 0,
                            gluon::Term::make_nil());
  if (j_result.is_error()) {
    printf("jump error: %s\n", j_result.get_error());
  }
  vm::vm_loop(&proc);
  // Print x0
  printf("Result X[0]=");
  proc.get_runtime_ctx().regs[0].println();

  return 0;
#endif
}
