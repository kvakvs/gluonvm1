#pragma once

#include "g_defs.h"
#include <cstdio>

namespace gluon {

namespace stdlib {
  template <typename... Args>
  inline void fmt(const char *format, Args&&... args) {
    std::printf(format, std::forward<Args>(args)...);
  }

  void abort(); // blow up stuff
  void sleep(word_t micro_sec);
} // ns stdlib

} // ns gluon
