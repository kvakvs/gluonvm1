#pragma once

#include "g_defs.h"
#include "g_term.h"
#include "g_heap.h"

namespace gluon {
namespace term {

class TupleBuilder {
  word_t m_arity;
  word_t m_index;
  Term *m_elements;
public:
  TupleBuilder(proc::Heap *heap, word_t arity): m_arity(arity), m_index(0) {
    m_elements = (Term *)heap->h_alloc(arity + 1);
  }
  inline void add(Term x) {
    G_ASSERT(m_index < m_arity);
    m_elements[++m_index] = x;
  }
  inline Term make_tuple() {
    G_ASSERT(m_index == m_arity)
    return Term::make_tuple(m_elements, m_arity);
  }
};

template <typename T> inline Term make_term(const T&);
template <> inline Term make_term(const char &x) {
  return Term::make_small_u((word_t)x);
}
template <> inline Term make_term(const word_t &x) {
  return Term::make_small_u(x);
}
template <> inline Term make_term(const sword_t &x) {
  return Term::make_small(x);
}

template <typename Iter> inline word_t length(Iter iter, Iter to) {
  word_t result = 0;
  for(; iter != to; iter++, result++) {
  }
  return result;
}
template <> inline word_t length<const char *>(const char *iter, const char *to) {
  return (word_t)(to - iter);
}

template <typename Iter>
Term build_list(proc::Heap *heap, Iter iter, Iter to) {
  if (iter == to) {
    return ::gluon::NIL;
  }

  word_t len = length(iter, to);
  printf("len=%zu\n", len);
  Term *h = (Term *)heap->h_alloc(2 * len);

  Term result = Term::make_cons(h);
  word_t i = 0;
  for(; iter != to; iter++) {
    h[0] = make_term(*iter);
    h[1] = (i == len - 1) ? ::gluon::NIL : Term::make_cons(h + 2);
    h += 2;
    i++;
  }

  return result;
}

Term build_string(proc::Heap *h, const char *cstr);

// Builds string as list of integers on heap
Term build_string(proc::Heap *heap, const Str &s);

} // ns term
} // ns gluon
