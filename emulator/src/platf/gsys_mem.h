#pragma once

#include <stdlib.h>
//#include "g_defs.h"
//#include "g_error.h"

namespace gluon {

namespace mem {

  template <class Type>
  class Blk {
  private:
    Type *mem_;
    size_t size_;
  public:
    Blk(Type *m, size_t size): mem_(m), size_(size) {}
    Type *mem() const { return mem_; }
    size_t size() const { return size_; }
  };

  // Uses new and delete to allocate system memory blocks
  // TODO: get rid of typed allocate and free here, use bytes
  class CppStdlibMemory {
  public:
    template <class Type>
    static Blk<Type> allocate() {
      return Blk<Type>(new Type, sizeof(Type));
    }

    template <class Type>
    static Blk<Type> allocate(size_t count) {
      return Blk<Type>(new Type[count], count * sizeof(Type));
    }

    template <class Type>
    static void deallocate(Blk<Type> &p) {
      if (p.size() == 1) {
        delete p.mem();
      } else {
        delete [] p.mem();
      }
    }

//    template <typename T>
//    static void free(T *p) {
//      // NOTE: this won't call no destructor
//      return free_bytes(reinterpret_cast<u8_t *>(p));
//    }
  };

} // ns mem

using system_memory = mem::CppStdlibMemory;

} // ns gluon
