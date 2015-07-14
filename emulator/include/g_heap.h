#pragma once

#include "g_defs.h"

namespace gluon {

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

  static void free_bytes(Heap *h, u8_t *p);

  template <typename T>
  void free(Heap *h, T *p) {
    // NOTE: does not call dtor
    return free_bytes(h, reinterpret_cast<u8_t *>(p));
  }
};

} // ns gluon
