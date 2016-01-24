#include "process_fail.h"
#include "predef_atoms.h"
#include "term_helpers.h"

namespace gluon {
namespace proc {

Term Fail::type_as_atom() const {
    switch (type_) {
        case FailType::Error:
            return atom::ERROR;
        case FailType::Exit:
            return atom::EXIT;
        case FailType::Throw:
            return atom::THROW;
        case FailType::None:;
    }
    return the_non_value;
}

void Fail::set(FailType ft, Term reason) {
    type_ = ft;
    value_ = reason;
}

FailType Fail::to_fail_type(Term type) {
    return (type == atom::THROW)
               ? FailType::Throw
               : ((type == atom::EXIT) ? FailType::Exit : FailType::Error);
}

void Fail::set_normal_exit() {
    clear();
    type(proc::FailType::Exit);
    value(atom::NORMAL);
}

void Fail::set_internal_error(Term v) {
    clear();
    panic_ = true;
    type(proc::FailType::Error);
    value(v);
}

void Fail::set_badarg(proc::Heap* heap, Term v) {
    clear();
    set(FailType::Error, term::make_tuple(heap, {atom::BADARG, v}));
}

void Fail::set_badarg() {
    clear();
    set(FailType::Error, atom::BADARG);
}

}  // ns proc
}  // ns gluon
