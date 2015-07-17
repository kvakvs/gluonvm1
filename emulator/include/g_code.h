#pragma once

#include "g_defs.h"
#include "g_term.h"
#include "g_error.h"
#include "g_reader.h"

namespace gluon {

using Labels = std::vector<word_t>;

class Code {
  Vector<Term> m_code;

  static const word_t MAX_ARITY = 7;
  // Opcode args are decoded here. This is not static for parallel processes
  Term            m_args[MAX_ARITY];

public:
  Code() {
  }

  // Scans raw code in bytes:sz, and builds jump table with processed args
  // TODO: this maybe belongs to g_gleam_loader.cpp
  MaybeError from_raw_gleam(const u8_t *bytes, word_t sz);

  inline void move(Code &src) {
    m_code = std::move(src.m_code);
  }

protected:
  Term read_arg_value(tool::Reader &r);
};

} // ns gluon
