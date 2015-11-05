#pragma once

#include "defs.h"
#include "term.h"
#include "heap.h"

#if G_DEBUG
#include "bif/bif_misc.h"
#endif

namespace gluon {
namespace term {

class TupleBuilder {
  Word m_arity;
  Word m_index;
  Term* m_elements;

 public:
  TupleBuilder(proc::Heap* heap, Word arity) : m_arity(arity), m_index(0) {
    m_elements = (Term*)heap->allocate<Word>(layout::Tuple::box_size(arity));
  }
  void add(Term x) {
    G_ASSERT(m_index < m_arity);
    layout::Tuple::element(m_elements, m_index) = x;
    m_index++;
  }
  Term make_tuple() {
    G_ASSERT(m_index == m_arity)
    return Term::make_tuple(m_elements, m_arity);
  }
};

template <typename T>
Term make_term(const T&);

template <>
inline Term make_term(const char& x) {
  return Term::make_small_u((Word)x);
}
template <>
inline Term make_term(const Word& x) {
  return Term::make_small_u(x);
}
template <>
inline Term make_term(const SWord& x) {
  return Term::make_small(x);
}
template <>
inline Term make_term(const Term& x) {
  return x;
}

template <typename Iter>
Word length(Iter iter, Iter to) {
  Word result = 0;
  for (; iter != to; iter++, result++) {
  }
  return result;
}
template <typename T>
inline Word length_p(T* iter, T* to) {
  return (Word)(to - iter + 1);
}

template <typename Iter>  // TODO: const Iter args?
Term build_list(proc::Heap* heap, Iter iter, Iter end) {
  if (iter == end) {
    return ::gluon::the_nil;
  }

  Word len = length_p(iter, end);
  Term* h = (Term*)heap->allocate<Word>(layout::Cons::box_word_size * len);

  Term result = Term::make_cons(h);
  for (; iter < end; iter++) {
    layout::Cons::head(h) = make_term(*iter);
    layout::Cons::tail(h) =
        iter + 1 == end ? ::gluon::the_nil
                        : Term::make_cons(h + layout::Cons::box_word_size);
    h += layout::Cons::box_word_size;
  }

#if G_DEBUG
  auto lresult = bif::length(result);
  G_ASSERT(lresult.is_proper == true);  // must be proper
#endif
  return result;
}

Term build_string(proc::Heap* h, const char* cstr);

// Builds string as list of integers on heap
Term build_string(proc::Heap* heap, const Str& s);

}  // ns term
}  // ns gluon
