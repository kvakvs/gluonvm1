#include "g_term.h"

namespace gluon {

word_t Term::g_zero_sized_tuple = 0;

#if FEATURE_MAPS
word_t Term::g_zero_sized_map = term_tag::BoxedMap::create_subtag(0);
#endif

} // ns gluon
