#include "g_heap.h"

namespace gluon {

u8_t *Heap::alloc_bytes(Heap *h, word_t bytes)
{
  if (!h) {
    return new u8_t[bytes];
  }
  G_TODO("g_heap::alloc");
  G_IF_NODEBUG(return nullptr;)
}

void Heap::free_bytes(Heap *h, u8_t *p)
{
  if (!h) {
    delete p;
    return;
  }
  G_TODO("g_heap::free");
}



} // ns gluon
