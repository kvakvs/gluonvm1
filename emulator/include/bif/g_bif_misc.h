#pragma once

#include "g_defs.h"
#include "g_term.h"
#include "g_functional.h"

namespace gluon {

class Process;

namespace bif {

bool is_term_smaller(Term a, Term b);
bool are_terms_equal(Term a, Term b, bool exact);
// Returns pair of {length, proper=true/improper=false}
Pair<word_t, bool> length(Term list);

// Attempts to call m:f with args
// Returns word_t * if code execution should jump there, or a Term if result is
// known immediately. Pass small integer (arity) in args if regs already
// contained args in the first 'arity' cells.
Either<word_t *, Term> apply(Process *proc, Term m, Term f, Term args,
                             Term *regs);

Term bif_apply_2(Process *, Term funobject, Term args);
Term bif_apply_3(Process *, Term m, Term f, Term args);
Term bif_atom_to_list_1(Process *, Term a);
Term bif_divide_2(Process *, Term a, Term b);
Term bif_equals_2(Process *, Term a, Term b);
Term bif_equals_exact_2(Process *, Term a, Term b);
Term bif_function_exported_3(Process *prc, Term m, Term f, Term arity);
Term bif_greater_equal_2(Process *, Term a, Term b);
Term bif_hd_1(Process *, Term a);
Term bif_integer_to_list_1(Process *, Term a);
Term bif_integer_to_list_2(Process *, Term a, Term base);
Term bif_length_1(Process *, Term a);
Term bif_less_equal_2(Process *, Term a, Term b);
Term bif_make_fun_3(Process *, Term m, Term f, Term arity);
Term bif_minus_2(Process *, Term a, Term b);
Term bif_multiply_2(Process *, Term a, Term b);
Term bif_plus_2(Process *, Term a, Term b);
Term bif_plusplus_2(Process *proc, Term a, Term b);
Term bif_tl_1(Process *, Term a);

} // ns bif
} // ns gluon
