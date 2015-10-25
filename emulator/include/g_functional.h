#pragma once

namespace gluon {

template <typename L, typename R>
class Either {
private:
  bool m_is_left;
  union { L m_left; R m_right; };
  Either() {}

public:
  Either(const L &l): m_is_left(true), m_left(l)   {}
  Either(const R &r): m_is_left(false), m_right(r) {}
  inline bool is_left() const { return m_is_left; }
  inline L &left() { G_ASSERT(is_left()); return m_left; }
  inline const L &left() const { G_ASSERT(is_left()); return m_left; }
  inline R &right() { G_ASSERT(!is_left()); return m_right; }
  inline const R &right() const { G_ASSERT(!is_left()); return m_right; }
};

template <class Mapping, typename Callable>
void for_each(Mapping &m, Callable fn) {
  while (m.have()) {
    fn(m.current());
    m.advance();
  }
}
} // ns gluon
