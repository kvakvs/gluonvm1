#pragma once

#include "g_defs.h"
#include "g_term.h"
#include "g_heap.h"

namespace gluon {
namespace term {

class TupleBuilder {
  Word m_arity;
  Word m_index;
  Term *m_elements;
public:
  TupleBuilder(proc::Heap *heap, Word arity): m_arity(arity), m_index(0) {
    m_elements = (Term *)heap->allocate<Word>(layout::TUPLE::box_size(arity));
  }
  inline void add(Term x) {
    G_ASSERT(m_index < m_arity);
    layout::TUPLE::element(m_elements, m_index) = x;
    m_index++;
  }
  inline Term make_tuple() {
    G_ASSERT(m_index == m_arity)
    return Term::make_tuple(m_elements, m_arity);
  }
};

template <typename T> inline Term make_term(const T&);
template <> inline Term make_term(const char &x) {
  return Term::make_small_u((Word)x);
}
template <> inline Term make_term(const Word &x) {
  return Term::make_small_u(x);
}
template <> inline Term make_term(const SWord &x) {
  return Term::make_small(x);
}

template <typename Iter> inline Word length(Iter iter, Iter to) {
  Word result = 0;
  for(; iter != to; iter++, result++) {
  }
  return result;
}
template <> inline Word length<const char *>(const char *iter, const char *to) {
  return (Word)(to - iter);
}

template <typename Iter>
Term build_list(proc::Heap *heap, Iter iter, Iter to) {
  if (iter == to) {
    return ::gluon::the_nil;
  }

  Word len = length(iter, to);
  Std::fmt("len=" FMT_UWORD "\n", len);
  Term *h = (Term *)heap->allocate<Word>(layout::CONS::BOX_SIZE * len);

  Term result = Term::make_cons(h);
  Word i = 0;
  for(; iter != to; iter++) {
    layout::CONS::head(h) = make_term(*iter);
    layout::CONS::tail(h) = (i == len - 1)
                            ? ::gluon::the_nil
                            : Term::make_cons(h + layout::CONS::BOX_SIZE);
    h += layout::CONS::BOX_SIZE;
    i++;
  }

  return result;
}

Term build_string(proc::Heap *h, const char *cstr);

// Builds string as list of integers on heap
Term build_string(proc::Heap *heap, const Str &s);

} // ns term
} // ns gluon
