#pragma once

#include <stdlib.h>
//#include "g_defs.h"
//#include "g_error.h"

namespace gluon {

namespace mem {

  class Blk {
  private:
    void *mem_;
    //size_t size_;
  public:
    Blk(void *m, size_t): mem_(m) {}
    void *mem() const { return mem_; }
    //size_t size() const { return size_; }
  };

  // Uses new and delete to allocate system memory blocks
  // TODO: get rid of typed allocate and free here, use bytes
  class CppStdlibMemory {
  public:
    static Blk allocate(size_t bytes);

//    template <typename T>
//    static T *alloc(Word count) {
//      // NOTE: this won't call no constructor
//      return reinterpret_cast<T*>(alloc_bytes(count * sizeof(T)));
//    }

    static void deallocate(Blk &p);

//    template <typename T>
//    static void free(T *p) {
//      // NOTE: this won't call no destructor
//      return free_bytes(reinterpret_cast<u8_t *>(p));
//    }
  };

} // ns mem

using system_memory = mem::CppStdlibMemory;

} // ns gluon
