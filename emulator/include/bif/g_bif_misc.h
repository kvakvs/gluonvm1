#pragma once

#include "g_defs.h"
#include "g_term.h"

namespace gluon {

class Process;

namespace bif {

bool is_term_smaller(Term a, Term b);

Term bif_minus_2(Process *, Term a, Term b);

} // ns bif
} // ns gluon