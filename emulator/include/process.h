#pragma once

#include "defs.h"
#include "term.h"
#include "error.h"
#include "heap.h"
#include "mailbox.h"
#include "functional.h"
#include "runtime_ctx.h"

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

enum class FailType {
  None,
  Exit,
  Error,
  Throw
};

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
  Word catch_level_ = 0;

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
    bool trap_exit = false;
  } pflags_;

  Term fail_value_ = the_non_value;
  proc::FailType fail_type_ = proc::FailType::None;

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

 public:
  Process() = delete;
  Process(VM& vm, Term gleader);

  VM& vm() { return vm_; }
  const VM& vm() const { return vm_; }

  void set_trap_exit(bool te) { pflags_.trap_exit = te; }

  Term get_registered_name() const { return reg_name_; }
  void registered_as(Term n) { reg_name_ = n; }
  void finished() { slice_result_ = proc::SliceResult::Finished; }
  proc::SliceResult get_slice_result() const { return slice_result_; }
  void set_slice_result(proc::SliceResult sr) { slice_result_ = sr; }

  Term get_pid() const {
    G_ASSERT(pid_.is_non_value() || pid_.is_pid());
    return pid_;
  }
  Term get_priority() const { return prio_; }
  erts::RuntimeContext& get_runtime_ctx() { return ctx_; }
  Term get_group_leader() const { return gleader_; }
  void set_group_leader(Term pid) { gleader_ = pid; }

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
  Term fail(proc::FailType ft, Term reason) {
    fail_type_  = ft;
    fail_value_ = reason;
    return the_non_value;
  }
  Term error(Term reason) {
    return fail(proc::FailType::Error, reason);
  }
  Term error(Term reason, const char* str);  // builds {ErrorTag, "text"}
  Term error(Term error_tag, Term reason);   // builds tuple {ErrorTag, Reason}
  Term error_badarg(Term reason);  // builds tuple {badarg, Reason}
  Term error_badarg();
  proc::FailType fail_type() const { return fail_type_; }
  Term fail_value() const { return fail_value_; }
  bool is_failed() const { return fail_value_.is_value(); }
  bool is_not_failed() const { return fail_value_.is_non_value(); }
  void clear_fail_state() {
    fail_type_ = proc::FailType::None;
    fail_value_ = the_non_value;
  }

  //
  // Send/receive thingies
  //
  void msg_send(Term pid, Term value);
  proc::Mailbox& mailbox() { return mailbox_; }
  const proc::Mailbox& mailbox() const { return mailbox_; }

  //
  // Execution control
  //
  // Copies args from proper list with precalculated length to registers
  void set_args(Term args, Word len);
  // Jumps to code saving current IP in CP. Make sure ctx is swapped out from
  // VM!
  void call(Word* code) {
    ctx_.assert_swapped_out_partial();
    ctx_.set_cp(ctx_.ip());
    jump(code);
  }
  void jump(Word* code) {
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
  Either<Word*, Term> apply(Term m, Term f, Term args, Term* regs);
};

#if G_TEST
void process_test(int argc, const char* argv[]);
#endif  // TEST

}  // ns gluon
