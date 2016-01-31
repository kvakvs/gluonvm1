#include "code_index.h"
#include "code_server.h"
#include "error.h"
#include "predef_atoms.h"
#include "process.h"
#include "vm.h"

#include <stdio.h>

#if G_TEST
#include "../test/test.h"
#endif

using namespace gluon;

int main(int argc, const char* argv[]) {
    VM vm;

#if G_TEST
    // test runner
    gluontest::run_tests(argc, argv);
#else

    // normal start
    vm.codeserver().path_append("../test");

    auto rootp = vm.root_process();
    vm.codeserver().load_module(rootp, vm.to_atom("g_test2"));
    // vm.codeserver().load_module(rootp, atom::ERLANG);

    // create root process and set it to some entry function
    Term start_args[1] = {
        the_nil,
    };
    // MFArity mfa(vm.to_atom("otp_ring0"), vm.to_atom("start"), 2);
    // MFArity mfa(vm.to_atom("init"), vm.to_atom("boot"), 1);
    MFArity mfa(vm.to_atom("g_test2"), vm.to_atom("test"), 0);

    rootp->spawn(mfa, start_args);

    // Run some code
    rootp->set_group_leader(rootp->get_pid());
    vm.vm_loop(false);

    // Print x0 as result
    libc::fmt("Result X[0]=");
    rootp->get_runtime_ctx().regs_[0].println(vm);

    return 0;
#endif
}
