#include "heap.h"
#include "fun.h"
#include "term.h"

namespace gluon {
namespace proc {
/*
Heap::Heap() {
  m_root = m_current = Node::create(DEFAULT_PROC_HEAP_WORDS);
  m_stack.put_stack(m_current, DEFAULT_PROC_STACK_WORDS);
}

Word *Heap::h_alloc(Word n)
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

Node *Node::create(Word sz_words) {
  Word *mem = new Word[sz_words];
  auto n = new (mem) Node;
  n->start = n->heap_start;
  n->limit = n->start + sz_words - sizeof(Node);
  return n;
}
*/

/*
void Stack::put_stack(Node *stk_node, Word size) {
  // Assume current node in heap has memory for stack
  G_ASSERT(stk_node->get_avail() >= size);
  m_node = stk_node;
  // Shrink node by stack size. Set 'top' to end of node's memory and 'bottom'
  // to new end (where stack will overflow).
  end_ = top_ = stk_node->limit;
  m_node->limit -= size;
  bottom_ = m_node->limit;
}
*/

// Takes all terms between 'start' and 'end', and copies them to 'dstheap', new
// resulting terms are placed in array 'dst' which should be large enough.
bool copy_terms(VM& vm,
                Heap* dstheap,
                const Term* start,
                const Term* end,
                Term* dst) {
    while (start < end) {
        *dst = copy_one_term(vm, dstheap, *start);
        start++;
        dst++;
    }
    return true;
}

// Copies one term 't' to 'dstheap' returns new clone term located in new heap
Term copy_one_term(VM& vm, Heap* dstheap, Term t) {
    // Immediate values go immediately out
    if (t.is_nonvalue() || t.is_nil() || t.is_small() || t.is_atom() ||
        t.is_short_pid() || t.is_short_port()) {
        return t;
    }
    if (t.is_tuple()) {
        Word arity = t.tuple_get_arity();
        Term* new_t =
            (Term*)dstheap->allocate<Word>(layout::Tuple::box_size(arity));
        Term* this_t = t.boxed_get_ptr<Term>();
        layout::Tuple::arity(new_t) = layout::Tuple::arity(this_t);
        // Deep clone
        for (Word i = 0; i < arity; ++i) {
            layout::Tuple::element(new_t, i) =
                copy_one_term(vm, dstheap, layout::Tuple::element(this_t, i));
        }
        return Term::make_tuple_prepared(new_t);
    }
    if (t.is_boxed()) {
        if (t.is_boxed_fun()) {
            BoxedFun* bf = t.boxed_get_ptr<BoxedFun>();
            return fun::box_fun(dstheap, bf->fun_entry, bf->pid, bf->frozen);
        }
    }
    t.println(vm);
    G_TODO("notimpl copy_one_term for some type of term");
}

}  // ns proc
}  // ns gluon
