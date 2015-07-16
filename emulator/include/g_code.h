#pragma once

#include "g_defs.h"
#include "g_term.h"

namespace gluon {

class Code {
  UniquePtr<const u8_t> m_code;
  word_t          m_size;

  static const word_t MAX_ARITY = 7;
  // Opcode args are decoded here. This is not static for parallel processes
  Term            m_args[MAX_ARITY];

public:
  Code() {
  }
  // Takes ownership of bytes arg
  inline void set(const u8_t *bytes, word_t sz) {
    m_code.reset(bytes);
    m_size = sz;
  }
  inline void move(Code &src) {
    m_code = std::move(src.m_code);
    m_size = src.m_size;
  }

  void read_args(u8_t *instr);
};

} // ns gluon
