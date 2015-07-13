#pragma once

#include "g_defs.h"
#include "g_error.h"

namespace gluon {

// Filesystem
namespace fs {

  class File {
  public:
    File() = delete;
    File(const Str &name);
    ~File();

    Result<word_t> size();
    Result<word_t> read(u8_t *dst, word_t bytes);
  };

} // ns fs

} // ns gluon
