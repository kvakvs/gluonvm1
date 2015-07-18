#pragma once

#include "g_defs.h"
#include "g_term.h"
#include "g_error.h"
#include "g_reader.h"

namespace gluon {

class Heap;

// A pair of atom and int arity, can be used as map key
typedef struct fun_arity_t {
  Term    fun;
  word_t  arity;
  static inline fun_arity_t create(Term f, word_t a) {
    fun_arity_t fa;
    fa.fun = f;
    fa.arity = a;
    return fa;
  }
  inline bool operator < (const fun_arity_t &other) const {
    // TODO: this may be not very correct
    return fun.value() < other.fun.value() || arity < other.arity;
  }
} fun_arity_t;

//
// Class Module represents a single Erlang module with code. When multiple
// versions of same module are loaded, you'll find one Module for each version
//
class Module {
public:
  typedef Map<word_t, code_offset_t> labels_t;
  typedef Vector<fun_arity_t> exports_t;
  typedef Map<fun_arity_t, label_index_t> funs_t;

private:
  Term m_name;

  labels_t  m_labels;
  exports_t m_exports; // just list of {f/arity}
  funs_t    m_funs; // map({f/arity} => label_index)

  // Instruction layout in code: { void *label; Term args[arity] }
  Vector<word_t> m_code;

public:
  Module(Term name, funs_t &funs, exports_t &exports)
    : m_name(name)
  {
    m_funs    = funs;
    m_exports = exports;
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
  Result<code_offset_t> resolve_function(Term f, word_t arity);

  // Resolves label to an offset in code
  Result<code_offset_t> resolve_label(label_index_t label);

  inline void set_code(Vector<word_t> &code) {
    m_code = std::move(code); // take ownership
  }
  inline void set_labels(labels_t &labels) {
    m_labels = labels;
  }

protected:
};

} // ns gluon
