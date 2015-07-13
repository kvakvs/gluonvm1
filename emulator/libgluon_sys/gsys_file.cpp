#include "g_sys_fs.h"
#include "g_error.h"

#include <stdio.h>

namespace gluon {
namespace fs {

  inline void *to_internal(FILE *f) { return reinterpret_cast<void *>(f); }
  inline FILE *to_file(void *f) { return reinterpret_cast<FILE *>(f); }

  File::File(void *f): m_handle(f) {
  }
  File::File(): m_handle(nullptr) {
  }

  MaybeError fs::File::open(const Str &name) {
    auto handle = ::fopen(name.c_str(), "rb");
    if (!handle) {
      return MaybeError("file open error");
    }
    m_handle = to_internal(handle);
    return success();
  }

  File::~File() {
    if (m_handle) {
      ::fclose(to_file(m_handle));
    }
  }

  Result<word_t> File::size() {
    auto f = to_file(m_handle);
    auto old_pos = ::ftell(f);

    ::fseek(f, 0, SEEK_END);
    auto result = static_cast<word_t>(::ftell(f));

    ::fseek(f, old_pos, SEEK_SET);
    return success(result);
  }

  Result<word_t> File::read(u8_t *dst, word_t bytes) {
    auto result = ::fread(dst, 1, bytes, to_file(m_handle));
    return success(result);
  }

} // ns fs
} // ns gluon
