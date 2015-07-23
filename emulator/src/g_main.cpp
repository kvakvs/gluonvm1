#include "g_vm.h"
#include "g_process.h"
#include "g_codeserver.h"
#include "g_error.h"

#include <stdio.h>

using namespace gluon;

#if G_TEST
static void run_tests(int argc, const char *argv[]) {
  term_test(argc, argv);
}
#endif

int main(int argc, const char *argv[]) {

  VM::init();

#if G_TEST
  // test runner
  run_tests(argc, argv);
  return 0;
#else

  // normal start
  //vm::load_module("../test/g_test1.S.gleam");
  CodeServer::path_append("../test");

  // create root process and set it to some entry function
  Process proc;
  auto j_result = proc.call(VM::to_atom("g_test1"),
                            VM::to_atom("test"),
                            0,
                            Term::make_nil());
  if (j_result.is_error()) {
    printf("jump error: %s\n", j_result.get_error());
  }

  // Run some code
  VM::vm_loop(&proc);

  // Print x0 as result
  printf("Result X[0]=");
  proc.get_runtime_ctx().regs[0].println();

  return 0;
#endif
}
