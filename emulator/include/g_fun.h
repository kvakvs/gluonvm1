#pragma once

#include "g_defs.h"
#include "g_term.h"

namespace gluon {

class fun_entry_t {
public:
  mfarity_t mfa;

  word_t  index;
  u32_t   uniq[4];
  word_t  old_index;
  word_t  old_uniq;

  word_t  num_free;   // how many extra terms with frozen values
  word_t  *code;

  fun_entry_t(): mfa(Term::make_nil(), Term::make_nil(), 0) {
  }
};

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wzero-length-array"
// TODO: pack this better in memory?
class boxed_fun_t {
public:
  // This struct will begin boxed memory at fun object location, followed by
  // 0 or several captured frozen terms (closure). First field includes
  // boxed subtag in 4 lower bits -- num_free:19,arity:8,subtag:4
  word_t hdr;
  Term pid;
  Term module;
  word_t index;
  u32_t  uniq[4];
  word_t old_index;
  word_t old_uniq;
  fun_entry_t *fe;
  Term frozen[0]; // captured terms (closure)
};
#pragma clang diagnostic pop

//
// Boxed callable functional object
//
class FunObject: public Term {
public:
  FunObject(word_t x): Term(x) {
    G_ASSERT(is_fun());
  }
  FunObject(Term &other): Term(other.as_word()) {
    G_ASSERT(is_fun());
  }

  //
  // Boxed callable object (a fun)
  //
  static inline FunObject make(boxed_fun_t *p) {
    return FunObject(term_tag::BoxedFun::create_from_ptr(p));
  }
  inline boxed_fun_t *get_object() const {
    return boxed_get_ptr<boxed_fun_t>();
  }
};

namespace fun {

// Fills words of memory mem with some fields from fun_entry_t and frozen terms.
// Returns boxable pointer. Mem should have enough words for boxed_fun_t and
// some extra for captured terms (closure).
// @args: fe - lambda entry from beam file, mem - will host boxed_fun_t and extra
// captured values, pid - oh well, its a pid; frozen - memory where we copy
// captured values from (pointer to registers basically)
boxed_fun_t *box_fun(fun_entry_t *fe, word_t *mem, Term pid, Term *frozen);

} // ns fun

} // ns gluon
