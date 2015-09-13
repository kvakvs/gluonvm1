#pragma once

#include "g_defs.h"
#include <cstdio>

namespace gluon {

namespace Std {
  //
  // Console output
  //
  void fmt(const char *s);

  template <typename... Args>
  inline void fmt(const char *format, Args&&... args) {
    std::printf(format, std::forward<Args>(args)...);
  }
  void puts();

  //
  // Self management
  //
  void abort(); // blow up stuff
  void exit(int); // blow up stuff with a return value

  //
  // Time and stuff
  //l
  void sleep(word_t micro_sec);
} // ns stdlib

} // ns gluon
