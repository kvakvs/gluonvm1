#pragma once

#include "g_defs.h"
#include "g_error.h"

namespace gluon {

namespace mem {
  Result<u8_t *> alloc_bytes(word_t bytes);
  template <typename T> Result<T *> alloc(word_t count) {
    // NOTE: this won't call no constructor
    return success(reinterpret_cast<T*>(alloc_bytes(count * sizeof(T))));
  }

  void free_bytes(u8_t *p);
  template <typename T> void free(T *p) {
    // NOTE: this won't call no destructor
    return free_bytes(reinterpret_cast<u8_t *>(p));
  }
} // ns mem

} // ns gluon
