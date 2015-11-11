#pragma once

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

} // ns gluon
