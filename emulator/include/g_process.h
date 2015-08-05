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
  Term    regs[VM_MAX_REGS];
#if FEATURE_FLOAT
  //float_t fp_regs[MAX_FP_REGS];
#endif
} runtime_ctx_t;

class ProcessStack {
  Vector<Term> cells;
public:
  ProcessStack() {}
  void push(Term t) {
    cells.push_back(t);
  }
  Term pop() {
    Term t = cells.back();
    cells.pop_back();
    return t;
  }
  void set_y(word_t offset, Term t) {
    cells[index_for_y(offset)] = t;
  }
  Term get_y(word_t offset) const {
    return cells[index_for_y(offset)];
  }
  void push_n_nils(word_t n) {
    if (!n) return;
    cells.reserve(cells.size() + n);
    for (word_t i = 0; i < n; ++i) {
      cells.push_back(NIL);
    }
  }
  void drop_n(word_t n) {
    G_ASSERT(cells.size() >= n);
    cells.resize(cells.size() - n);
  }
  word_t size() const {
    return cells.size();
  }

#if G_DEBUG
  void println();
#else
  inline void println() {}
#endif

protected:
  inline word_t index_for_y(word_t y) const {
    // when we work with stack, there's always CP hanging on top
    G_ASSERT(y < cells.size() - 1);
    return cells.size() - 2 - y;
  }
};

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
  ProcessStack  m_stack;
  ProcessHeap   m_heap;
  Term          m_pid = NONVALUE;
  mfarity_t     m_init_call;
  bool          m_trap_exit = false;
  // TODO: process dict
  Term          m_group_leader;
  Term          m_priority; // an atom: 'low', 'high' or 'normal'
  // TODO: result (is exiting, reason, put back in sched queue for reason)

  friend class Scheduler;
  void set_pid(Term pid) {
    m_pid = pid;
  }

public:
  Process() = delete;
  Process(Term gleader);
  Term get_pid() const {
    return m_pid;
  }
  Term get_priority() const {
    return m_priority;
  }
  runtime_ctx_t &get_runtime_ctx() {
    return m_ctx;
  }
  ProcessStack *get_stack() {
    return &m_stack;
  }

  //
  // Process memory thingies
  //

  ProcessHeap *get_heap() {
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

protected:
  // Resolves M:F/Arity and sets instruction pointer to it. Runs no code. Args
  // should be placed in registers before this process is scheduled to execute.
  MaybeError jump_to_mfa(mfarity_t &mfa);
};

#if G_TEST
void process_test(int argc, const char *argv[]);
#endif // TEST

} // ns gluon
