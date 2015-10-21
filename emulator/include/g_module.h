#pragma once

#include "g_defs.h"
#include "g_term.h"
#include "g_error.h"
#include "g_reader.h"
#include "g_fun.h"
#include "g_code_index.h"
#include "struct/g_array.h"

namespace gluon {

class Heap;

class export_t {
  // first field stores bits used to tag boxed value and boolean bif flag
  // (shifted left by BOXED_SUBTAG_BITS)
  word_t m_header;
  union {
    word_t *m_code;
    void   *m_bif_fn; // use VM::apply_bif with this and mfa.arity
  };

public:
  mfarity_t mfa;

  export_t() {}
  export_t(void *biffn)
    : m_header(term_tag::BoxedExport::create_subtag((word_t)(biffn != nullptr)))
    , m_bif_fn(biffn)
  {}
  export_t(word_t *_code, const mfarity_t &_mfa)
    : m_header(term_tag::BoxedExport::create_subtag((word_t)false)),
      m_code(_code), mfa(_mfa)
  {}

  static const word_t BIF_BIT = (1U << term_tag::BOXED_SUBTAG_BITS);
  inline bool is_bif() const {
    return (m_header & BIF_BIT) == BIF_BIT;
  }
  inline word_t *code() const {
    G_ASSERT(is_bif() == false);
    return m_code;
  }
  inline void *bif_fn() const {
    G_ASSERT(is_bif() == true);
    return m_bif_fn;
  }
};

//
// Class Module represents a single Erlang module with code. When multiple
// versions of same module are loaded, you'll find one Module for each version
//
class Module {
public:
  typedef Dict<word_t, word_t *>         labels_t;
  //typedef Map<fun_arity_t, fun_entry_t> funs_t;
  typedef Dict<fun_arity_t, export_t>    exports_t;
  typedef Vector<mfarity_t>             imports_t;
  typedef Vector<fun_entry_t>           lambdas_t;

#if FEATURE_LINE_NUMBERS
  typedef Vector<word_t>  line_refs_t;  // 24bit line + 8bit file index
  typedef Vector<Term>    file_names_t; // atoms with file names
#endif

private:
  Term      name_;
  labels_t  labels_;
  exports_t exports_; // just list of {f/arity}
  imports_t imports_;
  lambdas_t lambdas_;

#if FEATURE_CODE_RANGES
  // Map code range to fun/arity pair
  code::Index<fun_arity_t> fun_index_;
#endif
#if FEATURE_LINE_NUMBERS
  line_refs_t   m_line_refs;
  file_names_t  m_file_names;
#endif

  // Instruction layout in code: { void *label; Term args[arity] }
  Vector<word_t> code_;

public:
  Module(Term name, imports_t &imp)
    : name_(name)
  {
    imports_ = std::move(imp);
  }

  Module(Module &&src) {
    name_ = std::move(src.name_);
    code_ = std::move(src.code_);
  }

  Term get_name() const {
    G_ASSERT(name_.is_atom());
    return name_;
  }

  inline word_t read_word(word_t ptr) const {
    G_ASSERT(ptr < code_.size());
    return code_[ptr];
  }
  export_t *find_export(const fun_arity_t &fa) {
    return exports_.find_ptr(fa);
  }

  // Resolves function in current module to a code pointer
  // TODO: duplicates find_export, replace with fun table search or remove?
  //Result<word_t *> resolve_function(Term f, word_t arity);

  // Resolves label to a code pointer
  word_t *resolve_label(label_index_t label);

  inline void set_code(Vector<word_t> &code) {
    code_ = std::move(code); // take ownership
  }
  inline void set_labels(labels_t &labels) {
    labels_ = std::move(labels);
  }
  void set_exports(exports_t &e);
  void set_lambdas(lambdas_t &la) {
    lambdas_ = std::move(la);
  }
#if FEATURE_CODE_RANGES
  code::Range get_code_range();
  void set_fun_ranges(code::Index<fun_arity_t> &ci) {
    fun_index_ = std::move(ci);
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
    G_ASSERT(i < imports_.size());
    return &(imports_[i]);
  }
  fun_entry_t *get_lambda_entry(word_t i) {
    G_ASSERT(i < lambdas_.size());
    return &(lambdas_[i]);
  }

protected:
};

} // ns gluon
