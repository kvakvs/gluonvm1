#include "g_term.h"
#include "g_heap.h"

namespace gluon {

word_t Term::g_zero_sized_tuple = 0;

#if FEATURE_MAPS
word_t Term::g_zero_sized_map = term_tag::BoxedMap::create_subtag(0);
#endif

Term Term::allocate_cons(Heap *heap, Term head, Term tail) {
  Term *d = Heap::alloc<Term>(heap, 2);
  d[0] = head;
  d[1] = tail;
  return make_cons(d);
}

#if G_DEBUG
void Term::print()
{
  if (is_nil()) { printf("NIL\n"); }
  if (is_non_value()) { printf("NON_VALUE\n"); }
  if (is_atom()) { printf("ATOM#%zu\n", atom_val()); }
  if (is_boxed()) { printf("BOXED#%zu\n", boxed_get_subtag()); }
  if (is_cons()) { printf("CONS\n"); }
  printf("SOME UNKNOWN VALUE\n");
}
#endif


} // ns gluon
