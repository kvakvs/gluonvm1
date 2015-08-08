#include "g_heap.h"
#include "g_term.h"

namespace gluon {
namespace proc {

Heap::Heap() {
  m_root = m_current = Node::create(DEFAULT_PROC_HEAP_WORDS);
  m_stack.put_stack(m_current, DEFAULT_PROC_STACK_WORDS);
}

word_t *Heap::h_alloc(word_t n)
{
  if (m_current->get_avail() <= n) {
    auto node_size_power = std::min(m_node_count, HEAP_SEGMENT_GROWTH_MAX);
    // TODO: replace max here with proper calculation of nearest higher size
    auto node_size = std::max(DEFAULT_PROC_HEAP_WORDS << node_size_power,
                              n + Node::FIELDS_WORD_SIZE);

    // See constant comments
    auto new_node = Node::create(node_size);
    m_node_count++;
    m_current->next = new_node;
    m_current = new_node;
  }

  return m_current->allocate_words(n);
}

Node *Node::create(word_t sz_words) {
  word_t *mem = new word_t[sz_words];
  auto n = new (mem) Node;
  n->start = n->heap_start;
  n->limit = n->start + sz_words - sizeof(Node);
  return n;
}

void Stack::put_stack(Node *stk_node, word_t size) {
  // Assume current node in heap has memory for stack
  G_ASSERT(stk_node->get_avail() >= size);
  m_node = stk_node;
  // Shrink node by stack size. Set 'top' to end of node's memory and 'bottom'
  // to new end (where stack will overflow).
  m_end = m_top = stk_node->limit;
  m_node->limit -= size;
  m_bottom = m_node->limit;
}

void Stack::push_n_nils(word_t n) {
  G_ASSERT(get_avail() >= n);
  m_top -= n;
  std::fill_n(m_top, n, term::NIL);
}


} // ns proc
} // ns gluon
