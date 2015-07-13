#include "g_sys_mem.h"
#include "g_error.h"

namespace gluon {
namespace mem {

  Result<u8_t *> alloc_bytes(word_t bytes) {
    return success(new u8_t[bytes]);
  }

  void free_bytes(u8_t *p) {
    delete p;
  }

} // ns mem
} // ns gluon
