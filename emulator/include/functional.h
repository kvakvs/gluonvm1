#pragma once

#include <functional>

namespace gluon {

// Strongly typed pair of 2 any types. Only one can be active at any time
template <typename L, typename R>
class Either {
 private:
  bool m_is_left;
  union {
    L m_left;
    R m_right;
  };
  Either() {}

  // Either must be declared with different types for L and R
  static_assert(std::is_same<L, R>::value == false, "L must != R");

 public:
  Either(const L& l) : m_is_left(true), m_left(l) {}
  Either(const R& r) : m_is_left(false), m_right(r) {}
  bool is_left() const { return m_is_left; }
  L& left() {
    G_ASSERT(is_left());
    return m_left;
  }
  const L& left() const {
    G_ASSERT(is_left());
    return m_left;
  }
  R& right() {
    G_ASSERT(!is_left());
    return m_right;
  }
  const R& right() const {
    G_ASSERT(!is_left());
    return m_right;
  }
};

template <class Mapping, typename Callable>
void for_each(Mapping& m, Callable fn) {
  while (m.have()) {
    fn(m.current());
    m.advance();
  }
}
}  // ns gluon
