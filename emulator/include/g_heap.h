#pragma once

#include "g_defs.h"

namespace gluon {

class Term;

class Heap {
public:
  Heap() = delete;

  // h = nullptr will allocate on global C++ heap for debugging
  static u8_t *alloc_bytes(Heap *h, word_t bytes);

  template <typename T>
  static inline T *alloc(Heap *h, word_t count) {
    // NOTE: does not call constructors
    return reinterpret_cast<T *>(alloc_bytes(h, count * sizeof(T)));
  }

  template <typename T>
  static inline T *alloc_object(Heap *h) {
    // NOTE: will call constructor
    u8_t *bytes = alloc_bytes(h, sizeof(T));
    return new(bytes)T;
  }

  static void free_bytes(Heap *h, u8_t *p);

  template <typename T>
  static void free(Heap *h, T *p) {
    // NOTE: does not call dtor
    return free_bytes(h, reinterpret_cast<u8_t *>(p));
  }
  // Marks nested terms as unused (assuming no references to them)
  static void free_terms(Heap *h, Term *terms, word_t count) {
    // TODO: marking
    free(h, terms);
  }
};

} // ns gluon
