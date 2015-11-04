#pragma once

//#include "defs.h"
#include <functional>
#include <cstdint>
#include <cstdio>

namespace gluon {

namespace Std {
//
// Console output
//
void fmt(const char* s);

template <typename... Args>
void fmt(const char* format, Args&&... args) {
  std::printf(format, std::forward<Args>(args)...);
}
void puts();

//
// Self management
//
void abort();    // blow up stuff
void exit(int);  // blow up stuff with a return value

//
// Time and stuff
//
void sleep(size_t micro_sec);

void assert_fail(const char* what, const char* file, int line);

}  // ns stdlib

}  // ns gluon
