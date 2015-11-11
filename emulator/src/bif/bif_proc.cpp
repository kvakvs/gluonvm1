#include "bif/bif_proc.h"
#include "bif/bif_misc.h"
#include "process.h"
#include "predef_atoms.h"
#include "term_helpers.h"
#include "vm.h"

namespace gluon {
namespace bif {

Term bif_self_0(Process* proc) {
  return proc->get_pid();
}

static Term spawn_mfargs(Process* proc, Term m, Term f, Term args, bool link) {
  if (!m.is_atom()) {
    return proc->error_badarg(m);
  }
  if (!f.is_atom()) {
    return proc->error_badarg(f);
  }
  if (!args.is_list()) {
    return proc->error_badarg(args);
  }

  // TODO: on process control blocks' heap
  Process* new_proc = new Process(proc->vm(), proc->get_group_leader());
  MFArity mfa(m, f, bif::length(args).length);

  // A process (proc) spawning another process, and gives args from its heap
  // We should clone args to new process' registers
  Term old_heap_args[erts::max_fun_arity];
  Term new_heap_args[erts::max_fun_arity];  // clone of array_args in new heap

  args.cons_to_array(old_heap_args, sizeof(old_heap_args));

  proc::copy_terms(proc->vm(), new_proc->get_heap(), old_heap_args,
                   old_heap_args + mfa.arity, new_heap_args);

  try {
    mfa.println(proc->vm());
    new_proc->spawn(mfa, new_heap_args);
  } catch (std::runtime_error& e) {
    return proc->error(atom::ERROR, e.what());
  }

  if (link) {
    // TODO: Establish link in both directions
    proc->link(new_proc);
    new_proc->link(proc);
    // TODO: on error - destroy result
  }

  return new_proc->get_pid();
}

Term bif_spawn_3(Process* proc, Term m, Term f, Term args) {
  return spawn_mfargs(proc, m, f, args, false);
}

Term bif_spawn_link_3(Process* proc, Term m, Term f, Term args) {
  return spawn_mfargs(proc, m, f, args, true);
}

Term bif_group_leader_0(Process* proc) {
  return proc->get_group_leader();
}

Term bif_group_leader_2(Process* proc, Term pid, Term gl) {
  Process* other = proc->vm().scheduler().find(pid);
  other->set_group_leader(pid);
  return atom::OK;
}

Term bif_is_process_alive_1(Process* proc, Term pid) {
  if (!pid.is_short_pid()) {
    return proc->error_badarg(pid);
  }
  if (proc->vm().scheduler().find(pid) == nullptr) {
    return atom::FALSE;
  }
  return atom::TRUE;
}

Term bif_nif_error_1(Process* p, Term what) {
  return p->error(atom::ERROR, what);
}

Term bif_register_2(Process* p, Term name, Term pid_port) {
  if (!name.is_atom() || name == atom::UNDEFINED) {
    return p->error_badarg(name);
  }
  if (!pid_port.is_pid() && !pid_port.is_port()) {
    return p->error_badarg(pid_port);
  }
  switch (p->vm().register_name(name, pid_port)) {
    case RegisterResult::Ok:
      return atom::TRUE;
    case RegisterResult::RegistrationExists:  // fall through
    case RegisterResult::ProcessNotFound:
      return p->error_badarg(pid_port);
  }
}

Term bif_process_flag_2(Process* p, Term flag, Term value) {
  if (flag == atom::TRAP_EXIT) {
    if (!G_IS_BOOLEAN(value)) {
      return p->error_badarg(value);
    }
    p->set_trap_exit(value == atom::TRUE);
    return atom::OK;
  }
  return p->error_badarg(flag);
}

Term bif_exit_1(Process *p, Term what)
{
  return p->fail(proc::FailType::Exit, what);
}

Term bif_exit_2(Process *proc, Term pid, Term what)
{
  // If the first argument is not a pid, or a local port it is an error.

  if (pid.is_short_port()) {
    // TODO: check erts_port_synchronous_ops and lookup the port
    // TODO: call port exit with reason
    // TODO: check if exited, or if op was scheduled on port
    throw err::TODO("exit/2 for local port");
  }
  // not sure why?
  // TODO: if (pid.is_remote_port() && belongs to current node) return TRUE

  // If it is a remote pid, send a signal to the remote node.

  if (pid.is_remote_pid()) {
    // TODO: lookup the remote pid
    // TODO: if remote pid refers to current node: return TRUE
    // TODO: prepare distributed signal and send it if possible
    throw err::TODO("exit/2 for remote pid");
  } else if (!pid.is_short_pid()) {
    return proc->error_badarg();
  } else {
    // The pid is internal. Verify that it refers to an existing process.
    // TODO: Find process, honour locks (for SMP)
    Process* p = proc->vm().scheduler().find(pid);

    // TODO: Send an exit signal
    if (p) {
      throw err::TODO("notimpl exit signals");
    }

    // TODO: if sending to self - remove ERTS_PROC_LOCK_MAIN (whatever that is)
    // TODO: (smp) unlock

    // We may have exited ourselves and may have to take action.

    // TODO: if we killed self, do a nice exit: ERTS_BIF_CHK_EXITED(BIF_P)
    return atom::TRUE;
  }
}

}  // ns bif
}  // ns gluonl
