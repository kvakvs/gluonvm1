#include "g_heap.h"

namespace gluon {

word_t *ProcessHeap::h_alloc(word_t n)
{
  // TODO: grow or something
  G_ASSERT(m_htop + n < m_sp);

  auto result = m_htop;
  m_htop += n;
  return result;
}



} // ns gluon
