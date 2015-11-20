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

void Process::link(Process *other) {
  links_.push_back(other->get_pid());
}

void Process::finished() {
  slice_result_ = proc::SliceResult::Finished;
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

  Std::fmt("Process::jump_to_mfa -> %p\n", ctx_.ip().value());

  vm_.scheduler().add_new_runnable(this);
  return get_pid();
}

Term Process::bif_error(Term error_tag, Term reason) {
  return bif_fail(proc::FailType::Error,
                  term::make_tuple(get_heap(), {error_tag, reason}));
}

void Process::on_exception() {
  slice_result_ = proc::SliceResult::Exception;
}

// Exit signal behavior
//
// Exit signals are asynchronous. When the signal is received the receiver
// receives an 'EXIT' message if it is trapping exits; otherwise, it will either
// ignore the signal if the exit reason is normal, or go into an exiting state
// psflag_.exiting. When a process has gone into the exiting state it will not
// execute any more Erlang code, but it might take a while before it actually
// exits. The exit signal is being received when the 'EXIT' message is put in
// the message queue, the signal is dropped, or when it changes state into
// exiting. The time it is in the exiting state before actually exiting is
// undefined (it might take a really long time under certain conditions). The
// receiver of the exit signal does not break links or trigger monitors until
// it actually exits.
//
// Exit signals and other signals, e.g. messages, have to be received by a
// receiver in the same order as sent by a sender.
Process::ExitSigResult Process::send_exit_signal(
    Process* from, Term reason, ExitSigFlags flags, Term exit_tuple)
{
  // on 'kill', flip reason to 'killed'
  Term reason1 = (reason == atom::KILL) ? atom::KILLED : reason;
  G_ASSERT(reason.is_not_nonvalue());

  if ((pflags_.trap_exit && reason != atom::KILL) || flags.ignore_kill) {
    // TODO: trace update goes here
    if (exit_tuple.is_not_nonvalue()) {
      // TODO: special 'EXIT' message handler with locks?
      enqueue_message(exit_tuple);
    } else {
      enqueue_message(
            term::make_tuple(get_heap(),
                            {atom::EXIT, from->get_pid(), reason1})
            );
    }
    return ExitSigResult::MessageSent;
  } else if (reason == atom::NORMAL || flags.no_ignore_normal) {
    // TODO: SMP handling
    if (pflags_.exiting == false) {
      set_exiting(proc::copy_one_term(vm_, &heap_, reason));
      return ExitSigResult::WillExit;
    }
  }
  return ExitSigResult::NotAffected;
}

void Process::set_exiting(Term reason)
{
  // TODO: SMP locks?
  pflags_.suspended = false;
  pflags_.pending_exit = false;
  pflags_.exiting = true;
  pflags_.active = true;

  // this also will set slice result to clean up after next/current timeslice
  fail_set(proc::FailType::Exit, reason);

  catch_level_ = 0;

  ctx_.assert_swapped_out_partial();
  ctx_.set_ip(vm_.premade_instr(PremadeIndex::Error_exit_));
}

void Process::handle_error()
{
  // --- in handle error do
  // If fail_.arg_list? (parse tuple with error value and args)
  if (fail_.is_arg_list_set()) {
    throw err::TODO("fail_.arg_list_");
  }
  // If save trace flag set? Save stacktrace
  if (fail_.is_save_trace_set()) {
    throw err::TODO("fail_.save_trace_");
  }
  // If throw and catch level <= 0: convert Value into error:{nocatch, Value}
  if (catch_level_ <= 0 && fail_.type() == proc::FailType::Throw) {
    Term new_value = term::make_tuple(
          get_heap(), {atom::NOCATCH, fail_.value()}
          );
    fail_.set(proc::FailType::Error, new_value);
  }
  // TODO: call expand_error_value (for special values in OTP)

  // If catches > 0 || traced && !panic in reason. Panic ignores catches
  if (catch_level_ > 0 && !fail_.is_panic()) {
    // fill regs: [nonvalue, exception_tag, Value, p->ftrace]
    ctx_.regs_[0] = the_non_value;
    ctx_.regs_[1] = fail_.type_as_atom();
    ctx_.regs_[2] = fail_.value();
    ctx_.regs_[3] = fail_.last_trace();

    // new_ip=find next catch, cp=0, return new ip
    CodePointer new_ip = find_next_catch();
    // if still catches>0 return erl_exit catch not found
  } else { // terminate proc with Value
  }
  //throw err::TODO("handle error");
}

CodePointer Process::find_next_catch() {
  ctx_.assert_swapped_out();

  //For stack end to stack start do {
  //  if ptr=start return not found
  //  if !ptr.is_cp || (*ptr.value() not in return_[trace,to_trace,time_trace])
  //    && proc->cp) {
  //    cpp = proc->cp
  //    if cpp == beam_exc_trace...: ptr += 2
  //    elif cpp == beam_return_trace...: ptr += 2
  //    ... return_time_trace: ptr++
  //    ... return_to_trace: have_return_to_trace=true
  //  }
  //  while (ptr < stack start) {
  //    if ptr.is_catch {
  //      if active_catches (that is p->catch_count > 0) { goto found }
  //    } else if ptr.is_cp {
  //      prev = ptr
  //      if prev.cp_val == return_trace {
  //        ... magic
  //        ptr += 2
  //      } else is == return_to_Trace {
  //        ... magic
  //        ptr += 2
  //      } else is == return_time_trace {
  //        ... magic
  //        ptr++
  //      } else {
  //        if havereturn_to_trace {
  //          set it to false
  //          return_to_trace_ptr = ptr
  //        } else return_to_trace_ptr = nullptr
  //      }
  //    } else ptr++
  //  }
  //  return not found
  //  found:
  //    assert ptr still in stack
  //    proc->stop = prev
  //    if is_traced && return_to_trace_ptr {
  //      erts trace return to
  //    }
  //    else return ptr.catch_value
  //}
  //throw err::TODO("find next catch");
}

Term Process::bif_error(Term reason, const char* str) {
  Term err = term::build_string(get_heap(), str);
  return bif_error(reason, err);
}

void Process::send_message_to(Term pid, Term value) {
  // TODO: send to tuple {dst,node}, and to registered atom, and to port
  G_ASSERT(pid.is_pid());

  Process* other = vm_.scheduler().find(pid);
  if (!other) {
    Std::fmt(tRed("msg_send pid not found: "));
    pid.println(vm_);
    return;
  }
  return other->enqueue_message(value);
}

void Process::enqueue_message(Term value) {
  if (pflags_.exiting || pflags_.pending_exit) {
    return;
  }

  // Clone local value to value on remote heap
  Term dst_value = proc::copy_one_term(vm_, get_heap(), value);
  mailbox_.on_incoming(dst_value);
  vm_.scheduler().on_new_message(this);  // wake up receiver
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

void Process::set_args(Term args, Word len, BoxedFun *bf)
{
  set_args(args, len);
  auto num_frozen = bf->get_num_free();
  std::copy(bf->frozen, bf->frozen + num_frozen, ctx_.regs_ + len);
  ctx_.live += num_frozen;
}

Either<CodePointer, Term> Process::apply(Term m, Term f, Term args) {
  // Check the arguments which should be of the form apply(M,F,Args) where
  // F is an atom and Args is an arity long list of terms
  if (!f.is_atom()) {
    bif_error_badarg(f);  // fail right here
    return CodePointer();
  }

  // The module argument may be either an atom or an abstract module
  // (currently implemented using tuples, but this might change)
  Term _this = the_non_value;
  if (!m.is_atom()) {
    if (!m.is_tuple() || m.tuple_get_arity() < 1) {
      return bif_error_badarg(m);
    }
    // TODO: can optimize here by accessing tuple internals via pointer and
    // checking arity and then taking 2nd element
    _this = m;
    m = m.tuple_get_element(1);
    if (!m.is_atom()) {
      return bif_error_badarg(m);
    }
  }

  ctx_.assert_swapped_out();
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
        return bif_error(atom::SYSTEM_LIMIT);
      }
    }
    if (tmp.is_not_nil()) {  // Must be well-formed list
      return bif_error_badarg();
    }
    if (_this != the_non_value) {
      ctx_.regs_[arity++] = _this;
    }
  }

  // Get the index into the export table, or failing that the export
  // entry for the error handler.
  MFArity mfa(m, f, arity);

//  Std::fmt("process->apply (live=%zu) ", arity);
//  mfa.println(vm_);

  auto maybe_bif = vm_.find_bif(mfa);
  if (maybe_bif) {
    ctx_.live = 0;
    return vm_.apply_bif(this, arity, maybe_bif, ctx_.regs_);
  }

  Export* ep = vm_.codeserver().find_mfa(mfa);
  if (!ep) {
    // if ((ep = apply_setup_error_handler(proc, m, f, arity, regs)) == NULL)
    // goto error;
    return bif_error(atom::UNDEF);
  }
  if (ep->is_bif()) {
    ctx_.live = 0;
    return vm_.apply_bif(this, ep->mfa.arity, ep->bif_fn(), ctx_.regs_);
  }
  //  else if (ERTS_PROC_GET_SAVED_CALLS_BUF(proc)) {
  //      save_calls(proc, ep);
  //  }
  //  DTRACE_GLOBAL_CALL_FROM_EXPORT(proc, ep);
  //  return ep->addressv[erts_active_code_ix()];
  ctx_.live = arity;
  return ep->code();
}



}  // ns
