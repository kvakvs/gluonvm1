#pragma once

#include "g_term.h"

namespace gluon {

// Erl dist node implementation
class Node {
public:
  Term m_sysname = the_nil;
  dist::Creation m_creation = dist::internal_creation;
};

} // ns gluon
