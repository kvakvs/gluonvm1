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
  const Word* ip = nullptr;  // code pointer
  // continuation, works like return address for a single call. If more nested
  // calls are done, cp is saved to stack
  const Word* cp = nullptr;
  Word live = 0;  // saved registers count

  // TODO: maybe cache r0 in a local variable in vm loop?
  Term regs[erts::max_regs];
#if FEATURE_FLOAT
// Float fp_regs[vm::MAX_FP_REGS];
#endif
} RuntimeContext;

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
  Word catch_level_ = 0;
  Term bif_err_reason_ = the_non_value;

 protected:
  VM& vm_;
  RuntimeContext ctx_;
  proc::Heap heap_;
  Term pid_ = the_non_value;
  MFArity initial_call_;
  // TODO: process dict
  Term gleader_;
  Term prio_;  // an atom: 'low', 'high' or 'normal'

  struct {
    bool trap_exit = false;
  } pflags_;

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

  Term get_pid() const { return pid_; }
  Term get_priority() const { return prio_; }
  RuntimeContext& get_runtime_ctx() { return ctx_; }
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

  // Returns no_val and sets bif error flag in process. Use in bifs to signal
  // error condition: return proc->bif_error(reason);
  Term bif_error(Term reason);
  Term bif_error(Term reason, const char* str);  // builds {ErrorTag, "text"}
  Term bif_error(Term error_tag,
                 Term reason);   // builds tuple {ErrorTag, Reason}
  Term bif_badarg(Term reason);  // builds tuple {badarg, Reason}
  Term bif_badarg();

  //
  // Send/receive thingies
  //
  void msg_send(Term pid, Term value);
  proc::Mailbox& mailbox() { return mailbox_; }
  const proc::Mailbox& mailbox() const { return mailbox_; }
};

#if G_TEST
void process_test(int argc, const char* argv[]);
#endif  // TEST

}  // ns gluon
