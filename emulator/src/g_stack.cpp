#include "g_stack.h"

namespace gluon {
namespace proc {

void Stack::push_n_nils(Word n) {
  G_ASSERT(get_avail() >= n);
  top_ -= n;
  std::fill_n(top_, n, term::nil_as_word);
}

} // ns proc
} // ns gluon
