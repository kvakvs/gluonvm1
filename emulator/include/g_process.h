#pragma once

#include "g_defs.h"
#include "g_term.h"
#include "g_error.h"

namespace gluon {

class Module;

static const word_t MAX_REGS = 16; // max arity of fun which can be called
static const word_t MAX_FP_REGS = 2;

//------------------------------------------------------------------------------
// Thread of execution in VM
// Has own heap (well TODO, using shared now)
// Has own set of registers, own stack, own instruction pointer, mailbox and
// process dictionary
//------------------------------------------------------------------------------
class Process {
private:
  // Instruction pointer, relative to a given code object. Has no meaning
  // without a code object (a module pointer)
  code_ptr_t          m_ip;
  Vector<code_ptr_t>  m_call_stack;
  Term                m_x[MAX_REGS];
#if FEATURE_FLOAT
  //float_t           m_fp[MAX_FP_REGS];
#endif

public:
  Process() {
  }

  // Resolves M:F/Arity and sets instruction pointer to it. Runs no code.
  MaybeError call(Term m, Term f, word_t arity, Term args);

//  // Jump inside module
//  inline void jump_local(word_t offset) {
//    printf("local jump to %zu\n", offset);
//    m_ip.offset.value = offset;
//  }

  inline void set_x(word_t index, Term value) {
    G_ASSERT(index < MAX_REGS);
    m_x[index] = value;
  }

  // Not inlined, ask module for pointer to its code. Safety off!
  word_t *get_ip() const;
  word_t *get_code_base() const;

  // For special immed1 types (register and stack ref) convert them to their
  // values
  void vm_resolve_immed(Term &i) const {
    if (i.is_regx()) {
      i = m_x[i.regx_get_value()];
    }
//    else if (i.is_regy()) {
//      i = m_y[i.regy_get_value()];
//    }
#if FEATURE_FLOAT
    else if (i.is_regfp()) {
      i = m_fp[i.regy_get_value()];
    }
#endif
  }
};

} // ns gluon
