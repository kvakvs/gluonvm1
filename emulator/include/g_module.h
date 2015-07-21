#pragma once

#include "g_defs.h"
#include "g_term.h"
#include "g_error.h"
#include "g_reader.h"

namespace gluon {

class Heap;

// A pair of atom and int arity, can be used as map key
typedef Pair<Term, word_t> fun_arity_t;

//
// Class Module represents a single Erlang module with code. When multiple
// versions of same module are loaded, you'll find one Module for each version
//
class Module {
public:
  typedef Map<word_t, word_t> labels_t;
  typedef Vector<fun_arity_t> exports_t;
  typedef Map<fun_arity_t, label_index_t> funs_t;

private:
  Term m_name;

  labels_t  m_labels;
  exports_t m_exports; // just list of {f/arity}
  funs_t    m_funs; // map({f/arity} => label_index)

public:
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

  Term get_name() const {
    G_ASSERT(m_name.is_atom());
    return m_name;
  }

  inline word_t read_word(word_t ptr) const {
    G_ASSERT(ptr < m_code.size());
    return m_code[ptr];
  }

  // Resolves function in current module to a code pointer
  Result<word_t *> resolve_function(Term f, word_t arity);

  // Resolves label to a code pointer
  Result<word_t *> resolve_label(label_index_t label);

  inline void set_code(Vector<word_t> &code) {
    m_code = std::move(code); // take ownership
  }
  inline void set_labels(labels_t &labels) {
    m_labels = labels;
  }

protected:
};

} // ns gluon
