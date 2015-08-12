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

// Takes all terms between 'start' and 'end', and copies them to 'dstheap', new
// resulting terms are placed in array 'dst' which should be large enough.
bool copy_terms(Heap *dstheap, const Term *start, const Term *end, Term *dst)
{
  while (start < end) {
    *dst = copy_one_term(dstheap, *start);
    start++;
    dst++;
  }
  return true;
}

// Copies one term 't' to 'dstheap' returns new clone term located in new heap
Term copy_one_term(Heap *dstheap, Term t) {
  // Immediate values go immediately out
  if (t.is_non_value()
      || t.is_nil()
      || t.is_small()
      || t.is_atom()
      || t.is_short_pid()
      || t.is_short_port())
  {
    return t;
  }
  if (t.is_tuple()) {
    word_t arity   = t.tuple_get_arity();
    Term *new_t  = (Term *)dstheap->h_alloc(arity + 1);
    Term *this_t = t.boxed_get_ptr<Term>();
    new_t[0] = this_t[0];
    // Deep clone
    for (word_t i = 1; i <= arity; ++i) {
      new_t[i] = copy_one_term(dstheap, this_t[i]);
    }
    return Term::make_tuple_prepared(new_t);
  }
  t.println();
  G_TODO("notimpl clone");
}

} // ns proc
} // ns gluon
