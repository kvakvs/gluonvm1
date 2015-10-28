#pragma once

#include "g_defs.h"
#include "g_term.h"
#include "g_error.h"
#include "g_reader.h"
#include "g_fun.h"
#include "g_code_index.h"
#include "struct/g_array.h"

namespace gluon {

class VM;
class Heap;

class Export {
  // first field stores bits used to tag boxed value and boolean bif flag
  // (shifted left by BOXED_SUBTAG_BITS)
  Word hdr_;
  // either biffn or code must be not-null
  union {
    Word *code_;
    void *bif_fn_; // use VM::apply_bif with this and mfa.arity
  };

public:
  MFArity mfa;

  Export() {}
  Export(void *biffn)
    : hdr_(term_tag::BoxedExport::create_subtag((Word)(biffn != nullptr)))
    , bif_fn_(biffn)
  {
    G_ASSERT(biffn != nullptr); // either biffn or code must be not-null
  }
  Export(Word *co, const MFArity &_mfa)
    : hdr_(term_tag::BoxedExport::create_subtag((Word)false)),
      code_(co), mfa(_mfa)
  {
    G_ASSERT(co != nullptr); // either biffn or code must be not-null
  }

  static const Word BIF_BIT = (1U << term_tag::boxed_subtag_bits);
  inline bool is_bif() const {
    return (hdr_ & BIF_BIT) == BIF_BIT;
  }
  inline Word *code() const {
    G_ASSERT(is_bif() == false);
    return code_;
  }
  inline void *bif_fn() const {
    G_ASSERT(is_bif() == true);
    return bif_fn_;
  }
};

//
// Class Module represents a single Erlang module with code. When multiple
// versions of same module are loaded, you'll find one Module for each version
//
class Module {
public:
  using Labels  = Dict<Word, Word *>;
  using Exports = Dict<FunArity, Export>;
  using Imports = Vector<MFArity>;
  using Lambdas = Vector<FunEntry>;

  using LineRefs  = Vector<Word>; // 24bit line + 8bit file index
  using FileNames = Vector<Term>; // atoms with file names

private:
  VM      *vm_;
  Term    name_;
  Labels  labels_;
  Exports exports_; // just list of {f/arity}
  Imports imports_;
  Lambdas lambdas_;

#if FEATURE_CODE_RANGES
  // Map code range to fun/arity pair
  code::Index<FunArity> fun_index_;
#endif
#if FEATURE_LINE_NUMBERS
  LineRefs   line_refs_;
  FileNames  filenames_;
#endif

  // Instruction layout in code: { void *label; Term args[arity] }
  Vector<Word> code_;

public:
  Module(Term name, Imports &imp)
    : name_(name)
  {
    imports_ = std::move(imp);
  }

  Module(const Module &src) = delete;
  Module(Module &&src) = default;

  Term get_name() const {
    G_ASSERT(name_.is_atom());
    return name_;
  }

  inline Word read_word(Word ptr) const {
    G_ASSERT(ptr < code_.size());
    return code_[ptr];
  }
  Export *find_export(const FunArity &fa) {
    return exports_.find_ptr(fa);
  }

  // Resolves function in current module to a code pointer
  // TODO: duplicates find_export, replace with fun table search or remove?
  //Result<Word *> resolve_function(Term f, Word arity);

  // Resolves label to a code pointer
  Word *resolve_label(LabelIndex label);

  void set_code(Vector<Word> &code) {
    code_ = std::move(code); // take ownership
  }
  void set_labels(Labels &labels) {
    labels_ = std::move(labels);
  }
  void set_exports(Exports &e);
  void set_lambdas(Lambdas &la) {
    lambdas_ = std::move(la);
  }
#if FEATURE_CODE_RANGES
  code::Range get_code_range();
  void set_fun_ranges(code::Index<FunArity> &ci) {
    fun_index_ = std::move(ci);
  }
  bool find_fun_arity(Word *ptr, FunArity &out) const;
#endif

  void set_line_numbers(LineRefs &lr, FileNames &fn) {
    if (feature_line_numbers) {
      line_refs_ = std::move(lr);
      filenames_ = std::move(fn);
    }
  }

  MFArity *get_import_entry(Word i) {
    G_ASSERT(i < imports_.size());
    return &(imports_[i]);
  }
  FunEntry *get_lambda_entry(Word i) {
    G_ASSERT(i < lambdas_.size());
    return &(lambdas_[i]);
  }
};

} // ns gluon
