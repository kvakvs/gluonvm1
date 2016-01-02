#pragma once

#include "defs.h"
#include "platf/gsys_stdlib.h"
#include "struct/array.h"

namespace gluon {
namespace proc {

// Inverse stack iterator (for use with std::vector which grows up)
// Stack top is in the end of memory, stepping towards top will increase ptr
// Stepping back will decrease ptr.
// For classic stack implementation these ops will invert.
template <typename Value>
class StackIterator {
 private:
  Value* ptr_;

 public:
  using Self = StackIterator<Value>;

  explicit StackIterator(Value* p) : ptr_(p) {}

  const Value* pointer() const { return ptr_; }

  bool operator==(const Self& other) const { return ptr_ == other.ptr_; }
  bool operator!=(const Self& other) const { return ptr_ != other.ptr_; }
  bool operator>(const Self& other) const { return ptr_ > other.ptr_; }
  bool operator<(const Self& other) const { return ptr_ < other.ptr_; }
  bool operator<=(const Self& other) const { return ptr_ <= other.ptr_; }
  bool operator>=(const Self& other) const { return ptr_ >= other.ptr_; }

  bool operator>(const Value* other) const { return ptr_ > other; }
  bool operator<(const Value* other) const { return ptr_ < other; }

  Value& operator*() const { return *ptr_; }
  void step_towards_top() { ptr_++; }
  void step_towards_bottom() { ptr_--; }

  ssize_t operator-(const Self& other) const { return ptr_ - other.ptr_; }
};

#if 0
//
// Overlay Stack owns no memory and is located where owner places it
// Not growable. Does not actively defend its memory against overwrite.
//
class OverlayStack {
  // Node      *m_node = nullptr;  // where stack is
  Word* upper_limit_; // stack underflow mark
  Word* top_;         // stack tip, grows down from upper limit
  Word* lower_limit_; // stack bottom, delimits stack growth

 public:
  // TODO: Solve the fact that iter++ must advance towards stack bottom()
  using Iterator = StackIterator<Word>;
  using ConstIterator = StackIterator<const Word>;

 public:
  OverlayStack(Word* bottom, Word* top)
      : upper_limit_(top), top_(top), lower_limit_(bottom) {}

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
    G_ASSERT(top_ >= lower_limit_);
    return (Word)(top_ - lower_limit_);
  }
  Word get_used() const {
    G_ASSERT(top_ <= upper_limit_);
    return (Word)(upper_limit_ - top_);
  }
};
#endif  // 0

//
// Self-containing stack manages own memory using vector
// Data appends to end of vector, thus it expands up in memory
//
class SelfContainingStack {
 private:
  using Container = Vector<Word>;
  Container data_;

 public:
  // If iterator starts at top(), iter++ must advance towards stack bottom()
  // using Iterator = Container::RevIterator;
  // using ConstIterator = Container::ConstRevIterator;
  using Iterator = StackIterator<Word>;
  using ConstIterator = StackIterator<const Word>;

 public:
  // Where stack ends (most recent value stored)
  Iterator top() { return Iterator(&data_.back()); }
  ConstIterator top() const { return ConstIterator(&data_.back()); }

  // Special value to compare or pointer to one past oldest stack value (use !=
  // when iterating using reverse iterators)
  Iterator bottom() { return Iterator(&data_.front()); }
  ConstIterator bottom() const { return ConstIterator(&data_.front()); }

  void push(Word x) { data_.push_back(x); }
  Word pop() {
    Word t = data_.back();
    data_.resize(data_.size() - 1);
    return t;
  }
  // Set a stack cell (when code refers to a cell, there is always a CP value
  // stored at stack top, hence the extra 1 in offset)
  void set_y(Word index, Word value) {
    G_ASSERT(index < data_.size() - 1);
    data_[data_.size() - index - 2] = value;
  }
  // Read a stack cell (when code refers to a cell, there is always a CP value
  // stored at stack top, hence the extra 1 in offset)
  Word get_y(Word index) const {
    G_ASSERT(index < data_.size() - 1);
    return data_[data_.size() - index - 2];
  }
  void push_n_nils(Word n);
  void drop_n(Word n) { data_.resize(data_.size() - n); }

  // See if iterator belongs to the current stack
  template <typename Iter>
  bool contains(Iter& i) {
    // Because this vector-stack expands up in memory, top() is the upper end
    return top() >= i && bottom() <= i;
  }

  // Trim stack top to Iter position
  template <typename Iter>
  void trim(Iter& iter) {
    libc::fmt("trim top=%p bot=%p iter=%p\n", top().pointer(),
             bottom().pointer(), iter.pointer());
    G_ASSERT(contains(iter));
    // Can force to size_t because if i belongs to stack, difference with
    // bottom() will always be positive or 0 (this may bite in release build?)
    data_.resize((size_t)(iter - bottom()));
  }
};

using Stack = SelfContainingStack;

/*
// Check attempts to get member function addresses to ensure that interface,
// supported by a given class, is correct. Bad news is that it probably tries
// to instantiate unused code, and is in general ugly.
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
  static_assert(
      std::is_member_function_pointer<decltype(&C::push_n_nils)>::value,
      "push_n_nils is missing");
  static_assert(std::is_member_function_pointer<decltype(&C::drop_n)>::value,
                "drop_n is missing");
};

struct CheckStack1 : InterfaceCheck<SelfContainingStack> {};
struct CheckStack2 : InterfaceCheck<OverlayStack> {};
}  // ns check
*/

}  // ns proc
}  // ns gluon
