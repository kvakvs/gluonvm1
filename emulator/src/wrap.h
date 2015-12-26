#pragma once
#include "pointer.h"

namespace gluon {

// Wraps a type into a class to make it separate type
template <typename T>
class Wrap {
 private:
  T value_;

 public:
  Wrap() : value_() {}
  explicit Wrap(T x) : value_(x) {}
  T value() const { return value_; }
  void set_value(T newvalue) { value_ = newvalue; }
};

// Index in label table, wrapped to create a distinct compile-time type
struct LabelIndex : Wrap<Word> {
  LabelIndex() = default;
  explicit LabelIndex(Word x) : Wrap<Word>(x) {}
  explicit LabelIndex(const LabelIndex& other) = default;
  explicit LabelIndex(LabelIndex&& other) = default;
  LabelIndex& operator=(const LabelIndex&) = default;
  LabelIndex& operator=(LabelIndex&&) = default;
  bool operator <(const LabelIndex& other) const {
    return value() < other.value();
  }
};

// Pointer to code, wrapped to have a distinct type
struct CodePointer : Wrap<Word*> {
  CodePointer(): Wrap<Word *>(nullptr) {}
  explicit CodePointer(Word* x) : Wrap<Word*>(x) {}

  CodePointer(const CodePointer& other) = default;
  CodePointer(CodePointer&& other) = default;
  CodePointer& operator=(const CodePointer&) = default;
  CodePointer& operator=(CodePointer&&) = default;

  bool operator <(const CodePointer& other) const {
    return value() < other.value();
  }
  bool operator <=(const CodePointer& other) const {
    return value() <= other.value();
  }
  bool operator >(const CodePointer& other) const {
    return value() > other.value();
  }
  bool operator >=(const CodePointer& other) const {
    return value() >= other.value();
  }
  operator bool() const { return value() != nullptr; }

  template <typename Index>
  Word &operator[] (Index i) const { return value()[i]; }

  template <typename Offset>
  void operator += (Offset t) { set_value(value() + t); }

  void operator ++ (int) { set_value(value() + 1); }

  template <typename Offset>
  CodePointer operator + (Offset t) {
    return CodePointer(value() + t);
  }

  bool is_null() const { return value() == nullptr; }
  bool is_not_null() const { return value() != nullptr; }
};


//
// Continuation Pointer (CP) is a term-looking value tagged as Boxed in its
// low bits, and having PointerHTag::Continuation in its high bits.
// It is used as return address and also marks stack frame
//
class ContinuationPointer {
  Word value_;

public:
  explicit ContinuationPointer(Word x): value_(x) {}
  ContinuationPointer(): value_(0) {}

  void set_word(Word x) { value_ = x; }
  Word value() const { return value_; }

  // Is a valid continuation
  bool check() const {
    return check(value_);
  }

  static bool check(Word x) {
    return (x != 0)
        && PointerKnowledge::high_tag(x) == PointerHTag::Continuation
      #ifdef G_DEBUG
        && PointerKnowledge::low_tag(x) == PointerLTag::Boxed
      #endif
        ;
  }

  // Set highest bit to mark CP pushed on stack
  static ContinuationPointer make_cp(CodePointer x) {
    // just in case, hope word x is not already tagged
    G_ASSERT(false == ContinuationPointer((Word)x.value()).check());
    G_ASSERT(PointerKnowledge::is_userspace_pointer(x.value()));
    return ContinuationPointer(
          PointerKnowledge::set_tags(x.value(),
                                     PointerHTag::Continuation,
                                     PointerLTag::Boxed)
          );
  }

  // Check and clear highest bit to mark CP pushed on stack
  CodePointer untag() const {
    G_ASSERT(check() == true);
    auto result = PointerKnowledge::untag<Word*>(value_);
    G_ASSERT(PointerKnowledge::is_userspace_pointer(result));
    return CodePointer(result);
  }
};

} // ns gluon
