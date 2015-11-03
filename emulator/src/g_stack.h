#pragma once

#include "g_defs.h"
#include "struct/g_array.h"

namespace gluon {
namespace proc {

//
// Overlay Stack owns no memory and is located where owner places it
// Not growable. Does not actively defend its memory against overwrite.
//
class OverlayStack {
  // Node      *m_node = nullptr;  // where stack is
  Word* end_;     // stack underflow mark
  Word* top_;     // stack tip, grows down from heap end
  Word* bottom_;  // stack bottom, delimits stack growth

 public:
  OverlayStack(Word* bottom, Word* top) : end_(top), top_(top), bottom_(bottom) {}

  // Lowers 'limit' by 'size' words, puts stack there
  // void put_stack(Node *h_node, Word size);

  void set_y(Word index, Word value) {
    G_ASSERT(get_used() >= index + 1);
    top_[index + 1] = value;
  }
  Word get_y(Word index) const {
    G_ASSERT(get_used() >= index + 1);
    return top_[index + 1];
  }
  void push(Word x) {
    G_ASSERT(get_avail() > 0);
    top_--;
    *top_ = x;
  }
  Word pop() {
    G_ASSERT(get_used() > 0);
    auto result = *top_;
    top_++;
    return result;
  }
  void push_n_nils(Word n);
  void drop_n(Word n) {
    G_ASSERT(get_used() >= n);
    top_ += n;
  }
  Word get_avail() const {
    G_ASSERT(top_ >= bottom_);
    return (Word)(top_ - bottom_);
  }
  Word get_used() const {
    G_ASSERT(top_ <= end_);
    return (Word)(end_ - top_);
  }
};

//
// Self-containing stack manages own memory using vector
//
class SelfContainingStack {
private:
  Vector<Word> data_;
public:
};

using Stack = SelfContainingStack;

} // ns proc
} // ns gluon
