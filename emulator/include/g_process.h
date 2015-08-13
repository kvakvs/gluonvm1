#pragma once

#include "g_defs.h"
#include "g_term.h"
#include "g_error.h"
#include "g_heap.h"

namespace gluon {

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
  Term    regs[vm::MAX_REGS];
#if FEATURE_FLOAT
  //float_t fp_regs[vm::MAX_FP_REGS];
#endif
} runtime_ctx_t;


namespace proc {
  typedef enum {
    SR_NONE,
    SR_YIELD,       // process willingly gave up run queue
    SR_WAIT,        // process entered infinite or timed wait
    SR_FINISHED,    // process normally finished
    SR_EXCEPTION,   // error, exit or throw
  } slice_result_t;

  // Scheduler sets mark which is the current queue for this process
  typedef enum {
    Q_NONE,
    Q_PENDING_TIMERS,
    Q_HIGH,
    Q_NORMAL,
    Q_LOW,
    Q_TIMED_WAIT,
    Q_INF_WAIT,
  } sched_queue_t;

  const word_t WAIT_INFINITE = ~0UL;
} // ns proc

//------------------------------------------------------------------------------
// Thread of execution in VM
// Has own heap (well TODO, using shared now)
// Has own set of registers, own stack, own instruction pointer, mailbox and
// process dictionary
//------------------------------------------------------------------------------
class Process {
public:
  Term          m_stack_trace = NONVALUE;
  word_t        m_catch_level = 0;
  Term          m_bif_error_reason = NONVALUE;

protected:
  runtime_ctx_t m_ctx;
  //ProcessStack  m_stack; // TODO: remove merge with processheap
  proc::Heap    m_heap;
  Term          m_pid = NONVALUE;
  mfarity_t     m_init_call;
  bool          m_trap_exit = false;
  // TODO: process dict
  Term          m_group_leader;
  Term          m_priority; // an atom: 'low', 'high' or 'normal'
  // result after slice of CPU time is consumed or process yielded
  // (is exiting, reason, put back in sched queue for reason)
  // TODO: make this union to save space
  proc::slice_result_t m_slice_result = proc::SR_NONE;
  Term m_slice_result_reason = NONVALUE;
  word_t m_slice_result_wait = proc::WAIT_INFINITE;

  // Which queue we belong to
  proc::sched_queue_t  m_current_queue = proc::Q_NONE;

  friend class Scheduler;
  void set_pid(Term pid) {
    m_pid = pid;
  }

  //
  // Mailbox
  //
  List<Term> m_mbox; // Stuff arrives here, TODO: make this on process heap
  List<Term>::const_iterator m_mbox_ptr = m_mbox.end();
  // Set by recv_mark opcode and read by recv_set opcode
  word_t m_mbox_label;
  List<Term>::const_iterator m_mbox_saved;

public:
  Process() = delete;
  Process(Term gleader);

  void finished() {
    m_slice_result = proc::SR_FINISHED;
  }
  inline proc::slice_result_t get_slice_result() const {
    return m_slice_result;
  }
  inline void set_slice_result(proc::slice_result_t sr) { m_slice_result = sr; }

  Term get_pid() const {
    return m_pid;
  }
  Term get_priority() const {
    return m_priority;
  }
  runtime_ctx_t &get_runtime_ctx() {
    return m_ctx;
  }
  Term get_group_leader() const { return m_group_leader; }
  void set_group_leader(Term pid) { m_group_leader = pid; }

  //
  // Process memory thingies
  //

  proc::Heap *get_heap() {
    return &m_heap;
  }
  inline word_t *heap_alloc(word_t num_words) {
    return m_heap.h_alloc(num_words);
  }

  // Not inlined, ask module for pointer to its code. Safety off!
  word_t *get_ip() const;
  //  word_t *get_code_base() const;

  // Assumes that process already created on heap and initialized, assigns proc
  // to scheduler, assigns starting fun and args
  Result<Term> spawn(mfarity_t &mfa, Term *args);

  // Returns no_val and sets bif error flag in process. Use in bifs to signal
  // error condition: return proc->bif_error(reason);
  Term bif_error(Term reason);
  Term bif_error(Term reason, const char *str); // builds {ErrorTag, "text"}
  Term bif_error(Term error_tag, Term reason); // builds tuple {ErrorTag, Reason}
  Term bif_badarg(Term reason); // builds tuple {badarg, Reason}

  //
  // Send/receive thingies
  //
  void msg_send(Term pid, Term value);
  void incoming_send(Term value);
  Term msg_current();
  void msg_remove();
  void msg_next();
  // implementation of recv_mark opcode
  void msg_mark_(word_t);
  // implementation of recv_set opcode
  void msg_set_(word_t);

protected:
  // Resolves M:F/Arity and sets instruction pointer to it. Runs no code. Args
  // should be placed in registers before this process is scheduled to execute.
  MaybeError jump_to_mfa(mfarity_t &mfa);
};

#if G_TEST
void process_test(int argc, const char *argv[]);
#endif // TEST

} // ns gluon
