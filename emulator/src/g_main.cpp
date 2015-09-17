#include "g_vm.h"
#include "g_process.h"
#include "g_code_server.h"
#include "g_code_index.h"
#include "g_error.h"

#include <stdio.h>

#if G_TEST
#   include "../test/test.h"
#endif

using namespace gluon;

int main(int argc, const char *argv[]) {

  VM::init();

#if G_TEST
  // test runner
  gluontest::run_tests(argc, argv);
#else

  // normal start
  //vm::load_module("../test/g_test1.S.gleam");
  VM::get_cs()->path_append("../test");

  // create root process and set it to some entry function
  Term start_args[2] = {NIL, NIL};
  mfarity_t mfa(VM::to_atom("otp_ring0"), VM::to_atom("start"), 2);

  auto rootp = VM::g_root_proc;
  auto sp_result = rootp->spawn(mfa, start_args);
  if (sp_result.is_error()) {
    G_FAIL(sp_result.get_error());
  }

  // Run some code
  rootp->set_group_leader(rootp->get_pid());
  VM::vm_loop(false);

  // Print x0 as result
  Std::fmt("Result X[0]=");
  rootp->get_runtime_ctx().regs[0].println();

  return 0;
#endif
}
