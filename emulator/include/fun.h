#pragma once

#include "defs.h"
#include "term.h"

namespace gluon {

class FunEntry {
 public:
  MFArity mfa;

  Word index = 0;
  Uint32 uniq[4] = {0, 0, 0, 0};
  Word old_index = 0;
  Word old_uniq = 0;

  Word num_free = 0;  // how many extra terms with frozen values
  Word* code = nullptr;

  FunEntry() = default;
};

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wzero-length-array"
// TODO: pack this better in memory?
class BoxedFun {
 public:
  // This struct will begin boxed memory at fun object location, followed by
  // 0 or several captured frozen terms (closure). First field includes
  // boxed subtag in 4 lower bits -- num_free:19,arity:8,subtag:4
  Word hdr;
  Term pid;
  Term module;
  Word index;
  Uint32 uniq[4];
  Word old_index;
  Word old_uniq;
  FunEntry* fun_entry;
  Term frozen[0];  // captured terms (closure)

  Word get_arity() const { return (Uint8)(hdr >> 4); }
  Word get_num_free() const { return (hdr >> (4 + 8)) & 0x7ffff; }
};
#pragma clang diagnostic pop

//
// Boxed callable functional object
//
class FunObject : public Term {
 public:
  FunObject(Word x) : Term(x) { G_ASSERT(is_boxed_fun()); }
  FunObject(Term& other) : Term(other.value()) { G_ASSERT(is_boxed_fun()); }

  //
  // Boxed callable object (a fun)
  //
  static FunObject make(BoxedFun* p) {
    return FunObject(term_tag::BoxedFun::create_from_ptr(p));
  }
  BoxedFun* get_object() const { return boxed_get_ptr<BoxedFun>(); }
};

namespace fun {

// Fills words of memory mem with some fields from fun_entry_t and frozen terms.
// Returns boxable pointer. Mem should have enough words for BoxedFun and
// some extra for captured terms (closure).
// @args: fe - lambda entry from beam file, mem - will host BoxedFun and extra
// captured values, pid - oh well, its a pid; frozen - memory where we copy
// captured values from (pointer to registers basically)
Term box_fun(proc::Heap* heap, FunEntry* fe, Term pid, Term* frozen);
BoxedFun* box_fun(FunEntry* fe, Word* mem, Term pid, Term* frozen);

}  // ns fun

}  // ns gluon
