#pragma once

#include "g_defs.h"
#include "g_term.h"
#include "g_error.h"

namespace gluon {

class Module;

static const word_t MAX_REGS = 16; // max arity of fun which can be called
static const word_t MAX_FP_REGS = 2;

typedef struct gleam_ptr_t {
  Module        *module;
  code_offset_t  offset;

  gleam_ptr_t(): module(nullptr) {
    offset.value = 0;
  }
  inline bool is_good() const {
    return module != nullptr;
  }
  word_t next_word();
} gleam_ptr_t;

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
  gleam_ptr_t         m_ip;
  Vector<gleam_ptr_t> m_call_stack;
  Term                m_x[MAX_REGS];
#if FEATURE_FLOAT
  //float_t           m_fp[MAX_FP_REGS];
#endif

public:
  Process() {
  }

  // Resolves M:F/Arity and sets instruction pointer to it. Runs no code.
  MaybeError call(Term m, Term f, word_t arity, Term args);
  // Jump inside module
  inline void jump_local(word_t offset) {
    printf("local jump to %zu\n", offset);
    m_ip.offset.value = offset;
  }

  // Reads next instruction from code (instruction pointer)
  void *vm_fetch_instr() {
    printf("fetch instr at %zu\n", m_ip.offset.value);
    return reinterpret_cast<void *>(m_ip.next_word());
  }
  Term vm_fetch_term() {
    return Term(m_ip.next_word());
  }
  inline void set_x(word_t index, Term value) {
    G_ASSERT(index < MAX_REGS);
    m_x[index] = value;
  }
};

} // ns gluon
