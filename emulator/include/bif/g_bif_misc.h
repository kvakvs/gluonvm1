#pragma once

#include "g_defs.h"
#include "g_term.h"

namespace gluon {

class Process;

namespace bif {

bool is_term_smaller(Term a, Term b);
bool are_terms_equal(Term a, Term b, bool exact);

Term bif_make_fun_3(Process *, Term m, Term f, Term arity);
Term bif_minus_2(Process *, Term a, Term b);
Term bif_plus_2(Process *, Term a, Term b);
Term bif_multiply_2(Process *, Term a, Term b);
Term bif_divide_2(Process *, Term a, Term b);
Term bif_length_1(Process *, Term a);
Term bif_equals_2(Process *, Term a, Term b);
Term bif_equals_exact_2(Process *, Term a, Term b);
Term bif_less_equal_2(Process *, Term a, Term b);
Term bif_greater_equal_2(Process *, Term a, Term b);
Term bif_atom_to_list_1(Process *, Term a);
Term bif_integer_to_list_1(Process *, Term a);
Term bif_integer_to_list_2(Process *, Term a, Term base);

} // ns bif
} // ns gluon
