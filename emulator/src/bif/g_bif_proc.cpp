#include "bif/g_bif_proc.h"
#include "bif/g_bif_misc.h"
#include "g_process.h"
#include "g_predef_atoms.h"
#include "g_term_helpers.h"
#include "g_vm.h"

namespace gluon {
namespace bif {

Term bif_self_0(Process* proc) {
  return proc->get_pid();
}

static Term spawn_mfargs(Process* proc, Term m, Term f, Term args, bool link) {
  if (!m.is_atom()) {
    return proc->bif_badarg(m);
  }
  if (!f.is_atom()) {
    return proc->bif_badarg(f);
  }
  if (!args.is_list()) {
    return proc->bif_badarg(args);
  }

  // TODO: on process control blocks' heap
  Process* new_proc = new Process(proc->vm(), proc->get_group_leader());
  MFArity mfa(m, f, bif::length(args).first);

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
    return proc->bif_error(atom::ERROR, e.what());
  }

  if (link) {
    // TODO: Establish link in both directions
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
    return proc->bif_badarg(pid);
  }
  if (proc->vm().scheduler().find(pid) == nullptr) {
    return atom::FALSE;
  }
  return atom::TRUE;
}

Term bif_nif_error_1(Process* p, Term what) {
  return p->bif_error(atom::ERROR, what);
}

Term bif_register_2(Process* p, Term name, Term pid_port) {
  if (!name.is_atom() || name == atom::UNDEFINED) {
    return p->bif_badarg(name);
  }
  if (!pid_port.is_pid() && !pid_port.is_port()) {
    return p->bif_badarg(pid_port);
  }
  switch (p->vm().register_name(name, pid_port)) {
    case RegisterResult::Ok:
      return atom::TRUE;
    case RegisterResult::RegistrationExists:  // fall through
    case RegisterResult::ProcessNotFound:
      return p->bif_badarg(pid_port);
  }
}

Term bif_process_flag_2(Process* p, Term flag, Term value) {
  if (flag == atom::TRAP_EXIT) {
    if (!G_IS_BOOLEAN(value)) {
      return p->bif_badarg(value);
    }
    p->set_trap_exit(value == atom::TRUE);
    return atom::OK;
  }
  return p->bif_badarg(flag);
}

}  // ns bif
}  // ns gluonl
