#include "g_sys_stdlib.h"
#include "g_error.h"

#include <stdlib.h>
#include <unistd.h>

namespace gluon {
namespace stdlib {

  void abort() {
    ::abort();
  }

  void sleep(word_t micro_sec) {
    ::usleep(micro_sec);
  }

} // ns mem
} // ns gluon
