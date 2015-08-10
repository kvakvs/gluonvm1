#include "bif/g_bif_proc.h"
#include "bif/g_bif_misc.h"
#include "g_process.h"
#include "g_predef_atoms.h"
#include "g_term_helpers.h"
#include "g_vm.h"

namespace gluon {
namespace bif {

Term bif_self_0(Process *proc)
{
  return proc->get_pid();
}

Term bif_spawn_3(Process *proc, Term m, Term f, Term args)
{
  if (!m.is_atom()) { return proc->bif_badarg(m); }
  if (!f.is_atom()) { return proc->bif_badarg(f); }
  if (!args.is_list()) { return proc->bif_badarg(args); }
  // TODO: on process control blocks' heap
  Process *new_proc = new Process(proc->get_group_leader());
  mfarity_t mfa(m, f, bif::length(args).first);

  // A process (proc) spawning another process, and gives args from its heap
  // We should clone args to new process' registers
  Term old_heap_args[vm::MAX_FUN_ARITY];
  Term new_heap_args[vm::MAX_FUN_ARITY]; // clone of array_args in new heap
  args.cons_to_array(old_heap_args, sizeof(old_heap_args));
  proc::copy_terms(new_proc->get_heap(),
                   old_heap_args, old_heap_args + mfa.arity,
                   new_heap_args);

  auto sp_result = new_proc->spawn(mfa, new_heap_args);
  if (sp_result.is_error()) {
    return proc->bif_error(atom::ERROR, sp_result.get_error());
  }

  return new_proc->get_pid();
}

} // ns bif
} // ns gluonl
