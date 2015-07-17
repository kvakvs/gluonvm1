#pragma once

#include "g_defs.h"
#include "g_term.h"
#include "g_error.h"
#include "g_reader.h"

namespace gluon {

//
// Class Module represents a single Erlang module with code. When multiple
// versions of same module are loaded, you'll find one Module for each version
//
class Module {
  Term m_name;

  Vector<code_offset_t>    m_labels;
  Map<Term, label_index_t> m_functions;

  // Instruction layout in code: { void *label; Term args[arity] }
  Vector<word_t> m_code;

public:
  Module(Term name): m_name(name) {
  }

  Module(Module &&src) {
    m_name = std::move(src.m_name);
    m_code = std::move(src.m_code);
  }

  // Scans raw code in bytes:sz, and builds jump table with processed args
  // TODO: this maybe belongs to g_gleam_loader.cpp
  MaybeError from_raw_gleam(const u8_t *bytes, word_t sz);

  Term get_name() const {
    G_ASSERT(m_name.is_atom());
    return m_name;
  }

  inline word_t fetch_word(code_offset_t ip) const {
    G_ASSERT(ip.value < m_code.size());
    return m_code[ip.value];
  }

  // Resolves function in current module to a label number
  Result<code_offset_t> resolve_function(Term f);

  // Resolves label to an offset in code
  Result<code_offset_t> resolve_label(label_index_t label);

protected:
  Term read_arg_value(tool::Reader &r);
};

} // ns gluon
