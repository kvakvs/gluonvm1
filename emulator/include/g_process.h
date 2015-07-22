#pragma once

#include "g_defs.h"
#include "g_term.h"
#include "g_error.h"

namespace gluon {

class Module;
class Process;


static const word_t MAX_REGS = 16; // max arity of fun which can be called
static const word_t MAX_FP_REGS = 2;

// Set of stuff we take from Process struct to keep running, this will be saved
// by loop runner on context switch or loop end
typedef struct {
  Module *mod = nullptr;
  word_t *ip = nullptr;   // code pointer
  // continuation, works like return address for a single call. If more nested
  // calls are done, cp is saved to stack
  word_t *cp = nullptr;
  word_t live = 0; // saved registers count

  Term    regs[MAX_REGS];
#if FEATURE_FLOAT
  //float_t fp_regs[MAX_FP_REGS];
#endif
} runtime_ctx_t;

//------------------------------------------------------------------------------
// Thread of execution in VM
// Has own heap (well TODO, using shared now)
// Has own set of registers, own stack, own instruction pointer, mailbox and
// process dictionary
//------------------------------------------------------------------------------
class Process {
public:
  typedef Vector<Term> stack_t;
  Term    m_stack_trace = Term::make_non_value();

  runtime_ctx_t m_ctx;
  // TODO: Replace these with proper in-heap stack pointer, do not carry in ctx
  stack_t       m_stack;
  word_t        m_catch_level = 0;

public:
  Process() {
  }

  // Resolves M:F/Arity and sets instruction pointer to it. Runs no code.
  MaybeError call(Term m, Term f, word_t arity, Term args);
  runtime_ctx_t &get_runtime_ctx() {
    return m_ctx;
  }
  stack_t *get_stack() {
    return &m_stack;
  }
  Heap *get_heap();

//  // Jump inside module
//  inline void jump_local(word_t offset) {
//    printf("local jump to %zu\n", offset);
//    m_ip.offset.value = offset;
//  }

//  inline void set_x(word_t index, Term value) {
//    G_ASSERT(index < MAX_REGS);
//    m_x[index] = value;
//  }

  // Not inlined, ask module for pointer to its code. Safety off!
  word_t *get_ip() const;
  word_t *get_code_base() const;
};

} // ns gluon
