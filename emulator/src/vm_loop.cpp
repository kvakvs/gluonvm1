#include "vm.h"
#include "process.h"

// has own gluon namespace
#include "vm_impl.h"

namespace gluon {

void VM::vm_loop(bool init) {
  impl::VMRuntimeContext ctx(*this);
  void* jmp_to;
  Process* proc = nullptr;
  Scheduler& sched = VM::scheduler();
  impl::WantSchedule opcode_result = impl::WantSchedule::KeepGoing;

  if (init) {
    goto vm_jump_table_init;
  }

schedule:
  if (proc && opcode_result != impl::WantSchedule::Error) {
    // On error process will already be swapped out
    ctx.swap_out(proc);
  }
  proc = sched.next();
  if (!proc) {
    return;
  }                   // program finished?
  ctx.swap_in(proc);  // get copies of quick access data from environment

next_instr:
  jmp_to = (void*)(ctx.ip(0));

  // Print current Pid, MFA (if possible)
  ctx.println();
  libc::fmt(cBlue "[");
  // Std::fmt("[0x" FMT_0xHEX, (Word)ctx.ip);
  proc->get_pid().print(*this);
  libc::fmt(";");
  if (feature_code_ranges) {
    codeserver().print_mfa(ctx.ip());  // prints mfarity or pointer
  }
  libc::fmt("]: " cRst);

  ctx.inc_ip();
  ctx.vm_.assert_valid_vmloop_label(jmp_to);
  goto* jmp_to;

#include "vm_loop.inc.cpp"

vm_end:;
}
}  // ns gluon
