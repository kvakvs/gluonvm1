#pragma once

#include "g_defs.h"
#include "g_term.h"

namespace gluon {

class Process;

namespace bif {

bool is_term_smaller(Term a, Term b);
bool are_terms_equal(Term a, Term b, bool exact);

Term bif_minus_2(Process *, Term a, Term b);
Term bif_length_1(Process *, Term a);

} // ns bif
} // ns gluon
