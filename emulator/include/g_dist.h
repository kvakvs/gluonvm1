#pragma once

#include "g_term.h"

namespace gluon {

// Erl dist node implementation
class Node {
public:
  Term m_sysname = NIL;
  dist::creation_t m_creation = dist::INTERNAL_CREATION;
};

} // ns gluon
