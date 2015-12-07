#pragma once

#include "defs.h"
#include "term.h"
#include "error.h"
#include "heap.h"
#include "mailbox.h"
#include "functional.h"
#include "runtime_ctx.h"
#include "wrap.h"
#include "process_fail.h"

namespace gluon {

class VM;
class Module;
class Process;

namespace proc {
enum class SliceResult {
  None,
  Yield,      // process willingly gave up run queue
  Wait,       // process entered infinite or timed wait
  Finished,   // process normally finished
  Exception,  // error, exit or throw
};

// Scheduler sets mark which is the current queue for this process
enum class Queue {
  None,
  PendingTimers,
  High,
  Normal,
  Low,
  TimedWait,
  InfiniteWait,
};

constexpr Word wait_infinite = ~0UL;

}  // ns proc

//------------------------------------------------------------------------------
// Thread of execution in VM
// Has own heap (well TODO, using shared now)
// Has own set of registers, own stack, own instruction pointer, mailbox and
// process dictionary
//------------------------------------------------------------------------------
class Process {
 public:
  Term stack_trace_ = the_non_value;
  SWord catch_level_ = 0;

 private:
  VM& vm_;
  erts::RuntimeContext ctx_;
  proc::Heap heap_;
  Term pid_ = the_non_value;
  MFArity initial_call_;
  // TODO: process dict
  Term gleader_;
  Term prio_;  // an atom: 'low', 'high' or 'normal'

  struct {
    bool trap_exit: 1;  // exits are transformed into messages
    bool pending_exit: 1; // is waiting for own death
    bool exiting: 1;      // is on the death bed
    bool suspended: 1;  // does not get scheduler time-slice
    bool active: 1;
  } pflags_ = {false, false, false, false, false, };

  // result after slice of CPU time is consumed or process yielded
  // (is exiting, reason, put back in sched queue for reason)
  // TODO: make this union to save space
  proc::SliceResult slice_result_ = proc::SliceResult::None;
  Term slice_result_reason_ = the_non_value;
  Word slice_result_wait_ = proc::wait_infinite;

  // Which queue we belong to
  proc::Queue current_queue_ = proc::Queue::None;

  friend class Scheduler;
  void set_pid(Term pid) { pid_ = pid; }

  proc::Mailbox mailbox_;
  Term reg_name_ = the_non_value;

  //
  // Links and monitors
  //
  SingleList<Term> links_;
  proc::Fail fail_;

 public:
  Process() = delete;
  Process(VM& vm, Term gleader);

  VM& vm() { return vm_; }
  const VM& vm() const { return vm_; }

  void link(Process *other);
  void set_trap_exit(bool te) { pflags_.trap_exit = te; }

  Term get_registered_name() const { return reg_name_; }
  void registered_as(Term n) { reg_name_ = n; }
  void finished();
  proc::SliceResult get_slice_result() const { return slice_result_; }
  void set_slice_result(proc::SliceResult sr) { slice_result_ = sr; }

  Term get_pid() const {
    G_ASSERT(pid_.is_nonvalue() || pid_.is_pid());
    return pid_;
  }
  Term get_priority() const { return prio_; }
  erts::RuntimeContext& get_runtime_ctx() { return ctx_; }
  Term get_group_leader() const { return gleader_; }
  void set_group_leader(Term pid) { gleader_ = pid; }

  // Called by scheduler to inform proess that its got slice of CPU time
  // to reset internal variables
  void new_slice() {
  }

  //
  // Process memory thingies
  //

  proc::Heap* get_heap() { return &heap_; }
  Word* heap_alloc(Word num_words) { return heap_.allocate<Word>(num_words); }

  // Not inlined, ask module for pointer to its code. Safety off!
  Word* get_ip() const;
  //  Word *get_code_base() const;

  // Assumes that process already created on heap and initialized, assigns proc
  // to scheduler, assigns starting fun and args
  Term spawn(MFArity& mfa, Term* args);

  //
  // Failures -- ERRORS THROWS EXITS
  //

  // Returns no_val and sets bif error flag in process. Use in bifs to signal
  // error condition: return proc->bif_error(reason);
  void fail_set(proc::FailType ft, Term reason) {
    fail_.set(ft, reason);
    on_exception();
  }
  Term bif_fail(proc::FailType ft, Term reason) {
    fail_set(ft, reason);
    return the_non_value;
  }
  Term bif_error(Term reason) {
    return bif_fail(proc::FailType::Error, reason);
  }
  Term bif_error(Term reason, const char* str);  // builds {ErrorTag, "text"}
  Term bif_error(Term error_tag, Term reason);   // builds tuple {ErrorTag, Reason}
  Term bif_error_badarg(Term reason) {
    fail_.set_badarg(get_heap(), reason);
    on_exception();
    return the_non_value;
  }  // builds tuple {badarg, Reason}
  Term bif_error_badarg() {
    fail_.set_badarg();
    on_exception();
    return the_non_value;
  }
private:
  // Called after exception or exit been called
  void on_exception();

  // Accessors to private fail_ object
public:
  bool is_failed() const { return fail_.is_failed(); }
  bool is_not_failed() const { return fail_.is_not_failed(); }
  void fail_clear() { fail_.clear(); }
  auto fail_value() const { return fail_.value(); }

  typedef struct {
    bool ignore_kill: 1;
    bool no_ignore_normal: 1;
  } ExitSigFlags;
  enum class ExitSigResult {
    MessageSent,
    WillExit,
    NotAffected
  };
  ExitSigResult send_exit_signal(
      Process* from, Term reason, ExitSigFlags flags, Term exit_tuple);
  // Reason must belong to this process heap
  void set_exiting(Term reason);
  // Checks error condition in fail_, checks flag, unrolls stack, exits, throws
  // or panicks depending on situation
  void handle_error();
private:
  CodePointer find_next_catch();
  CodePointer catch_jump_to(Word index);

public:
  //
  // Send/receive thingies
  //
  // Finds destination pid and informs it about incoming value
  void send_message_to(Term pid, Term value);
  // Places message into own msgbox, v is copied on current proc heap
  void enqueue_message(Term v);
  proc::Mailbox& mailbox() { return mailbox_; }
  const proc::Mailbox& mailbox() const { return mailbox_; }

  //
  // Execution control
  //
  // Copies args from proper list with precalculated length to registers
  void set_args(Term args, Word len);
  // Same as set_args PLUS copies frozen values from bf closure
  void set_args(Term args, Word len, BoxedFun* bf);
  // Jumps to code saving current IP in CP. Make sure ctx is swapped out
  // from VM!
  void call(CodePointer code) {
    ctx_.assert_swapped_out_partial();
    ctx_.set_cp(ctx_.ip());
    jump(code);
  }
  void jump(CodePointer code) {
    ctx_.assert_swapped_out_partial();
    ctx_.set_ip(code);
  }

  // Prepares (attempts) to call m:f with args, m is atom or tuple pair
  // Returns one of:
  // *  Word* position in code; Code is not executed by apply just a code
  //    pointer is returned to continue execution from there;
  // *  Term if result is known immediately.
  // Pass small integer (arity) in args if regs already contain args in the
  // first 'arity' cells.
  Either<CodePointer, Term> apply(Term m, Term f, Term args);
};

#if G_TEST
void process_test(int argc, const char* argv[]);
#endif  // TEST

}  // ns gluon
