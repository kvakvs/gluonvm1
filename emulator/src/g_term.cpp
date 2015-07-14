#include "g_term.h"
#include "g_heap.h"

namespace gluon {

word_t Term::g_zero_sized_tuple = 0;

Term Term::allocate_cons(Heap *heap, Term head, Term tail) {
  Term *d = Heap::alloc<Term>(heap, 2);
  d[0] = head;
  d[1] = tail;
  return make_cons(d);
}


#if FEATURE_MAPS
word_t Term::g_zero_sized_map = term_tag::BoxedMap::create_subtag(0);
#endif

} // ns gluon
