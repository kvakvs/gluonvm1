#pragma once

#include <array>
#include <vector>
#include "defs.h"

namespace gluon {

namespace containers {

// Wraps STL-compatible vector or fixed size array. Reimplement as appropriate.
template <class ContainerType>
class STLArray {
 private:
  using Self = STLArray<ContainerType>;
  ContainerType data_;

 public:
  using Iterator = typename ContainerType::iterator;
  using ConstIterator = typename ContainerType::const_iterator;
  using RevIterator = typename ContainerType::reverse_iterator;
  using ConstRevIterator = typename ContainerType::const_reverse_iterator;
  using Value = typename ContainerType::value_type;

  size_t size() const { return data_.size(); }

  Value& operator[](size_t i) {
    G_ASSERT(i < size());
    return data_[i];
  }
  const Value& operator[](size_t i) const {
    G_ASSERT(i < size());
    return data_[i];
  }

  // Storage access
  Value* data() { return data_.data(); }
  const Value* data() const { return data_.data(); }

  Value& front() { return data_.front(); }
  const Value& front() const { return data_.front(); }

  Value& back() { return data_.back(); }
  const Value& back() const { return data_.back(); }

  Iterator begin() { return data_.begin(); }
  Iterator end() { return data_.end(); }
  ConstIterator begin() const { return data_.begin(); }
  ConstIterator end() const { return data_.end(); }

  RevIterator rbegin() { return data_.rbegin(); }
  RevIterator rend() { return data_.rend(); }
  ConstRevIterator rbegin() const { return data_.rbegin(); }
  ConstRevIterator rend() const { return data_.rend(); }

  void resize(size_t sz) { data_.resize(sz); }
  void reserve(size_t sz) { data_.reserve(sz); }

  void push_back(const Value& x) { data_.push_back(x); }
};

}  // ns containers

// A fixed array
template <typename Val, size_t Sz>
using Array = containers::STLArray<std::array<Val, Sz>>;

// A growable vector
template <typename Val>
using Vector = containers::STLArray<std::vector<Val>>;

// Non-owning pointer to a window of memory
template <class T>
class ArrayView {
 private:
  T* start_ = nullptr;
  T* end_ = nullptr;

 public:
  ArrayView() {}
  ArrayView(T* start, size_t sz) : start_(start), end_(start + sz) {}

  T* data() const { return start_; }
  T* limit() const { return end_; }
  size_t size() const {
    G_ASSERT(end_ >= start_);
    return (size_t)(end_ - start_);
  }
};

}  // ns gluon
