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
  void push(Word x) { data_.push_back(x); }
  Word pop() {
    Word t = data_.back();
    data_.resize(data_.size() - 1);
    return t;
  }
  void set_y(Word index, Word value) {
    G_ASSERT(index < data_.size());
    data_[data_.size() - index - 2] = value;
  }
  Word get_y(Word index) const {
    G_ASSERT(index < data_.size());
    return data_[data_.size() - index - 2];
  }
  void push_n_nils(Word n);
  void drop_n(Word n) {
    data_.resize(data_.size() - n);
  }
};

using Stack = SelfContainingStack;

namespace check {

template <class C>
class InterfaceCheck {
  static_assert(std::is_member_function_pointer<decltype(&C::push)>::value,
                "push is missing");
//  using push_t = void(*)(Word);
//  using maybe_push_t = decltype(&C::push);
//  static_assert(std::is_same<maybe_push_t, push_t>::value,
//                "push is missing");

  static_assert(std::is_member_function_pointer<decltype(&C::pop)>::value,
                "pop is missing");
  static_assert(std::is_member_function_pointer<decltype(&C::get_y)>::value,
                "get_y is missing");
  static_assert(std::is_member_function_pointer<decltype(&C::set_y)>::value,
                "set_y is missing");
  static_assert(std::is_member_function_pointer<decltype(&C::push_n_nils)>::value,
                "push_n_nils is missing");
  static_assert(std::is_member_function_pointer<decltype(&C::drop_n)>::value,
                "drop_n is missing");
};

struct CheckStack1: InterfaceCheck<SelfContainingStack> {};
struct CheckStack2: InterfaceCheck<OverlayStack> {};
} // ns check

} // ns proc
} // ns gluon
