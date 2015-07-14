#include "g_heap.h"

namespace gluon {

u8_t *Heap::alloc_bytes(Heap *h, word_t bytes)
{
  if (!h) {
    return new u8_t[bytes];
  }
  G_TODO("heaps! alloc");
}

void Heap::free_bytes(Heap *h, u8_t *p)
{
  if (!h) {
    delete p;
  }
  G_TODO("heaps! free");
}



} // ns gluon
