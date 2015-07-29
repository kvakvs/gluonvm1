#pragma once

#include "g_defs.h"
#include "g_term.h"
#include "g_error.h"
#include "g_reader.h"
#include "g_fun.h"
#include "g_code_index.h"

namespace gluon {

class Heap;

//
// Class Module represents a single Erlang module with code. When multiple
// versions of same module are loaded, you'll find one Module for each version
//
class Module {
public:
  typedef Map<word_t, word_t *>         labels_t;
  //typedef Map<fun_arity_t, fun_entry_t> funs_t;
  typedef Map<fun_arity_t, word_t *>    exports_t;
  typedef Vector<mfarity_t>             imports_t;
  typedef Vector<fun_entry_t>           lambdas_t;

#if FEATURE_LINE_NUMBERS
  typedef Vector<word_t>  line_refs_t;  // 24bit line + 8bit file index
  typedef Vector<Term>    file_names_t; // atoms with file names
#endif

private:
  Term      m_name;
  labels_t  m_labels;
  exports_t m_exports; // just list of {f/arity}
  imports_t m_imports;
  lambdas_t m_lambdas;

#if FEATURE_CODE_RANGES
  // Map code range to fun/arity pair
  code::Index<fun_arity_t> m_fun_index;
#endif
#if FEATURE_LINE_NUMBERS
  line_refs_t   m_line_refs;
  file_names_t  m_file_names;
#endif

public:
  // Instruction layout in code: { void *label; Term args[arity] }
  Vector<word_t> m_code;

public:
  Module(Term name, imports_t &imp)
    : m_name(name)
  {
    m_imports = std::move(imp);
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
  void set_exports(exports_t e) {
    m_exports = e;
  }
  void set_lambdas(lambdas_t &la) {
    m_lambdas = std::move(la);
  }
#if FEATURE_CODE_RANGES
  code::Range get_code_range();
  void set_fun_ranges(code::Index<fun_arity_t> &ci) {
    m_fun_index = std::move(ci);
  }
  fun_arity_t find_fun_arity(word_t *ptr) const;
#endif

#if FEATURE_LINE_NUMBERS
  void set_line_numbers(line_refs_t &lr, file_names_t &fn) {
    m_line_refs = std::move(lr);
    m_file_names = std::move(fn);
  }
#endif

  mfarity_t *get_import_entry(word_t i) {
    G_ASSERT(i < m_imports.size());
    return &(m_imports[i]);
  }
  fun_entry_t *get_lambda_entry(word_t i) {
    G_ASSERT(i < m_lambdas.size());
    return &(m_lambdas[i]);
  }

protected:
};

} // ns gluon
