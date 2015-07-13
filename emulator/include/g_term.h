#pragma once

#include "g_defs.h"

namespace gluon {

// This wrap is here to make strong type difference between hardware hw::Word
// (which is just a machine size unsigned integer) and term type, which is
// complex bitfield structure.
// Note: Wrapping an integer with class is efficient if all members are inline
// and optimizations are on.
class Term {
private:
  hw::Word m_val;

public:
  constexpr Term(hw::Word v): m_val(v) {}
  constexpr Term(): m_val(0) {}
  constexpr Term(const Term &other): m_val(other.m_val) {}

};

} // ns gluon
