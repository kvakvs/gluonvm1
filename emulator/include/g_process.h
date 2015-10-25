#pragma once

#include "g_defs.h"
#include "g_term.h"
#include "g_error.h"
#include "g_heap.h"
#include "g_mailbox.h"

namespace gluon {

class VM;
class Module;
class Process;

// Set of stuff we take from Process struct to keep running, this will be saved
// by loop runner on context switch or loop end
typedef struct {
//  Module *mod = nullptr;
  word_t *ip = nullptr;   // code pointer
  // continuation, works like return address for a single call. If more nested
  // calls are done, cp is saved to stack
  word_t *cp = nullptr;
  word_t live = 0; // saved registers count

  // TODO: maybe cache r0 in a local variable in vm loop?
  Term    regs[erts::max_regs];
#if FEATURE_FLOAT
  //float_t fp_regs[vm::MAX_FP_REGS];
#endif
} runtime_ctx_t;


namespace proc {
  enum SliceResult {
    None,
    Yield,       // process willingly gave up run queue
    Wait,        // process entered infinite or timed wait
    Finished,    // process normally finished
    Exception,   // error, exit or throw
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

  constexpr word_t wait_infinite = ~0UL;
} // ns proc

//------------------------------------------------------------------------------
// Thread of execution in VM
// Has own heap (well TODO, using shared now)
// Has own set of registers, own stack, own instruction pointer, mailbox and
// process dictionary
//------------------------------------------------------------------------------
class Process {
public:
  Term          stack_trace_ = NONVALUE;
  word_t        catch_level_ = 0;
  Term          bif_err_reason_ = NONVALUE;

protected:
  VM            &vm_;
  runtime_ctx_t ctx_;
  proc::Heap    heap_;
  Term          pid_ = NONVALUE;
  mfarity_t     initial_call_;
  // TODO: process dict
  Term          gleader_;
  Term          prio_; // an atom: 'low', 'high' or 'normal'

  struct {
    bool trap_exit = false;
  } pflags_;

  // result after slice of CPU time is consumed or process yielded
  // (is exiting, reason, put back in sched queue for reason)
  // TODO: make this union to save space
  proc::SliceResult slice_result_ = proc::SliceResult::None;
  Term              slice_result_reason_ = NONVALUE;
  word_t            slice_result_wait_ = proc::wait_infinite;

  // Which queue we belong to
  proc::Queue       current_queue_ = proc::Queue::None;

  friend class Scheduler;
  void set_pid(Term pid) {
    pid_ = pid;
  }

  proc::Mailbox mailbox_;
  Term          reg_name_ = NONVALUE;

public:
  Process() = delete;
  Process(VM &vm, Term gleader);

  VM &vm() { return vm_; }
  const VM &vm() const { return vm_; }

  void set_trap_exit(bool te) {
    pflags_.trap_exit = te;
  }

  Term get_registered_name() const {
    return reg_name_;
  }
  void registered_as(Term n) {
    reg_name_ = n;
  }
  void finished() {
    slice_result_ = proc::SliceResult::Finished;
  }
  inline proc::SliceResult get_slice_result() const {
    return slice_result_;
  }
  inline void set_slice_result(proc::SliceResult sr) { slice_result_ = sr; }

  Term get_pid() const {
    return pid_;
  }
  Term get_priority() const {
    return prio_;
  }
  runtime_ctx_t &get_runtime_ctx() {
    return ctx_;
  }
  Term get_group_leader() const { return gleader_; }
  void set_group_leader(Term pid) { gleader_ = pid; }

  //
  // Process memory thingies
  //

  proc::Heap *get_heap() {
    return &heap_;
  }
  word_t *heap_alloc(word_t num_words) {
    return heap_.allocate<word_t>(num_words);
  }

  // Not inlined, ask module for pointer to its code. Safety off!
  word_t *get_ip() const;
  //  word_t *get_code_base() const;

  // Assumes that process already created on heap and initialized, assigns proc
  // to scheduler, assigns starting fun and args
  Term spawn(mfarity_t &mfa, Term *args);

  // Returns no_val and sets bif error flag in process. Use in bifs to signal
  // error condition: return proc->bif_error(reason);
  Term bif_error(Term reason);
  Term bif_error(Term reason, const char *str); // builds {ErrorTag, "text"}
  Term bif_error(Term error_tag, Term reason); // builds tuple {ErrorTag, Reason}
  Term bif_badarg(Term reason); // builds tuple {badarg, Reason}
  Term bif_badarg();

  //
  // Send/receive thingies
  //
  void msg_send(Term pid, Term value);
  proc::Mailbox &mailbox() { return mailbox_; }
  const proc::Mailbox &mailbox() const { return mailbox_; }

protected:
  // Resolves M:F/Arity and sets instruction pointer to it. Runs no code. Args
  // should be placed in registers before this process is scheduled to execute.
  void jump_to_mfa(mfarity_t &mfa);
};

#if G_TEST
void process_test(int argc, const char *argv[]);
#endif // TEST

} // ns gluon
