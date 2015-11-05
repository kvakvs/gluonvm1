#include "stack.h"
#include "term.h"

namespace gluon {
namespace proc {

void OverlayStack::push_n_nils(Word n) {
  G_ASSERT(get_avail() >= n);
  top_ -= n;
  std::fill_n(top_, n, term::nil_as_word);
}

void SelfContainingStack::push_n_nils(Word n) {
  data_.reserve(data_.size() + n);
  for (Word i = 0; i < n; ++i) {
    push(term::nil_as_word);
  }
}

}  // ns proc
}  // ns gluon
