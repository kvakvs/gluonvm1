#pragma once

#include "defs.h"
//#include "error.h"
#include "struct/g_str.h"

namespace gluon {

// Filesystem
namespace fs {

class File {
 public:
  File();
  File(void*);
  ~File();

  void open(const Str& name);
  // May leave read position at file end, re-seek after you used size
  Word size();
  void seek(Word offset);
  Word read(Uint8* dst, Word bytes);
  bool is_good() { return !(is_error() || is_eof()); }
  bool is_error();
  bool is_eof();

 private:
  void* m_handle;
};

bool exists(const Str&);

}  // ns fs

}  // ns gluon
