#include "gsys_stdlib.h"
#include "defs.h"

#include <stdlib.h>
#include <unistd.h>

namespace gluon {
namespace libc {

void abort() {
  ::abort();
}

void sleep(size_t micro_sec) {
  ::usleep((unsigned int)micro_sec);
}

void exit(int x) {
  ::exit(x);
}

void fmt(const char* s) {
  while (*s)
    std::putchar(*s++);
}

void puts() {
  std::puts("");
}

void assert_fail(const char* what, const char* file, int line) {
  libc::fmt(cRed cBold "FAIL: %s" cRst " (%s:%d)\n", what, file, line);
  libc::abort();
}

}  // ns Std
}  // ns gluon
