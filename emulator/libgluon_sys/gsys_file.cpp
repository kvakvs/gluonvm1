#include "g_sys_fs.h"
#include "g_error.h"

namespace gluon {
namespace fs {

  File::File(const Str &name) {

  }

  File::~File() {

  }

  Result<word_t> File::size() {
    return error<word_t>("notimpl");
  }

  Result<word_t> File::read(u8_t *dst, word_t bytes) {
    return error<word_t>("notimpl");
  }

} // ns fs
} // ns gluon
