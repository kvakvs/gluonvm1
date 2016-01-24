#pragma once

//#include <memory>
#include <stdlib.h>

namespace gluon {

namespace mem {

template <class Type>
class Blk {
   private:
    Type* mem_;
    size_t size_;

   public:
    Blk(Type* m, size_t size) : mem_(m), size_(size) {}
    Type* mem() const { return mem_; }
    size_t size() const { return size_; }
    //~Blk() { G_ASSERT(mem == nullptr); }
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
    static void deallocate(Blk<Type>& p) {
        if (p.size() == 1) {
            delete p.mem();
        } else {
            delete[] p.mem();
        }
    }
};

}  // ns mem

using SysMemory = mem::CppStdlibMemory;

}  // ns gluon
