#include "process.h"
#include "code_server.h"
#include "code.h"
#include "module.h"
#include "vm.h"
#include "error.h"
#include "predef_atoms.h"
#include "term_helpers.h"

namespace gluon {

Process::Process(VM& vm, Term gleader) : vm_(vm), gleader_(gleader) {
  prio_ = atom::NORMAL;
}

Term Process::spawn(MFArity& mfa, Term* args) {
  // Check that we aren't on any scheduler yet
  G_ASSERT(false == pid_.is_pid());

  initial_call_ = mfa;

  // Set regs to M,F,A and jump to apply_mfargs_
  ctx_.arg_regs_[0] = mfa.mod;
  ctx_.arg_regs_[1] = mfa.fun;
  ctx_.arg_regs_[2] = term::build_list(get_heap(), args, args + mfa.arity);
  ctx_.live = 0;

  // Precondition: Registers should be set to execute apply call
  ctx_.set_cp(vm_.premade_instr(PremadeIndex::Normal_exit_));
  ctx_.set_ip(vm_.premade_instr(PremadeIndex::Apply_mfargs_));

  Std::fmt("Process::jump_to_mfa -> " FMT_0xHEX "\n", (Word)ctx_.ip());

  vm_.scheduler().add_new_runnable(this);
  return get_pid();
}

Term Process::error(Term error_tag, Term reason) {
  term::TupleBuilder tb(get_heap(), 2);
  tb.add(error_tag);
  tb.add(reason);
  return fail(proc::FailType::Error, tb.make_tuple());
}

Term Process::error(Term reason, const char* str) {
  Term err = term::build_string(get_heap(), str);
  return error(reason, err);
}

Term Process::error_badarg(Term reason) {
  return error(atom::BADARG, reason);
}

Term Process::error_badarg() {
  return error(atom::BADARG);
}

void Process::msg_send(Term pid, Term value) {
  // TODO: send to tuple {dst,node}, and to registered atom, and to port
  G_ASSERT(pid.is_pid());

  Process* other = vm_.scheduler().find(pid);
  if (!other) {
    Std::fmt(tRed("msg_send pid not found: "));
    pid.println(vm_);
    return;
  }
  // Clone local value to value on remote heap
  Term dst_value = proc::copy_one_term(vm_, other->get_heap(), value);
  other->mailbox().on_incoming(dst_value);
  vm_.scheduler().on_new_message(other);  // wake up receiver
}

void Process::set_args(Term args, Word len) {
  ctx_.assert_swapped_out();
  ctx_.live = len;
  Term* reg = &ctx_.regs_[0];
  while (args.is_cons()) {
    args.cons_head_tail(*reg, args);
    reg++;
  }
}

Either<Word*, Term> Process::apply(Term m,
                                   Term f,
                                   Term args) {
  // Check the arguments which should be of the form apply(M,F,Args) where
  // F is an atom and Args is an arity long list of terms
  if (!f.is_atom()) {
    error_badarg(f);  // fail right here
    return nullptr;
  }

  // The module argument may be either an atom or an abstract module
  // (currently implemented using tuples, but this might change)
  Term _this = the_non_value;
  if (!m.is_atom()) {
    if (!m.is_tuple() || m.tuple_get_arity() < 1) {
      error_badarg(m);
      return nullptr;
    }
    // TODO: can optimize here by accessing tuple internals via pointer and
    // checking arity and then taking 2nd element
    _this = m;
    m = m.tuple_get_element(1);
    if (!m.is_atom()) {
      error_badarg(m);
      return nullptr;
    }
  }

  ctx_.assert_swapped_out_partial();
  Word arity = 0;
  if (args.is_small()) {
    // Small unsigned in args means args already are loaded in regs
    arity = args.small_word();
  } else {
    // Walk down the 3rd parameter of apply (the argument list) and copy
    // the parameters to the x registers (regs[]). If the module argument
    // was an abstract module, add 1 to the function arity and put the
    // module argument in the n+1st x register as a THIS reference.
    Term tmp = args;
    while (tmp.is_cons()) {
      if (arity < erts::max_regs - 1) {
        tmp.cons_head_tail(ctx_.regs_[arity++], tmp);
      } else {
        error(atom::SYSTEM_LIMIT);
        return nullptr;
      }
    }
    if (tmp.is_not_nil()) {  // Must be well-formed list
      error_badarg();
      return nullptr;
    }
    if (_this != the_non_value) {
      ctx_.regs_[arity++] = _this;
    }
  }
  ctx_.live = arity;

  // Get the index into the export table, or failing that the export
  // entry for the error handler.
  MFArity mfa(m, f, arity);

  auto maybe_bif = vm_.find_bif(mfa);
  if (maybe_bif) {
    return vm_.apply_bif(this, mfa.arity, maybe_bif, ctx_.regs_);
  }

  Export* ep = vm_.codeserver().find_mfa(mfa);
  if (!ep) {
    // if ((ep = apply_setup_error_handler(proc, m, f, arity, regs)) == NULL)
    // goto error;
    error(atom::UNDEF);
    return nullptr;
  }
  if (ep->is_bif()) {
    return vm_.apply_bif(this, ep->mfa.arity, ep->bif_fn(), ctx_.regs_);
  }
  //  else if (ERTS_PROC_GET_SAVED_CALLS_BUF(proc)) {
  //      save_calls(proc, ep);
  //  }
  //  DTRACE_GLOBAL_CALL_FROM_EXPORT(proc, ep);
  //  return ep->addressv[erts_active_code_ix()];
  return ep->code();
}

}  // ns
