#include "process_fail.h"
#include "predef_atoms.h"
#include "term_helpers.h"

namespace gluon {
namespace proc {

void Fail::set(Term type, Term reason)
{
  if (type == atom::THROW) {
    return set(FailType::Throw, reason);
  } else if (type == atom::EXIT) {
    return set(FailType::Exit, reason);
  }
  return set(FailType::Error, reason);
}

void Fail::set(FailType ft, Term reason) {
  type_ = ft;
  value_ = reason;
}

void Fail::set_normal_exit() {
  clear();
  type(proc::FailType::Exit);
  value(atom::NORMAL);
}

void Fail::set_internal_error(Term v)
{
  clear();
  panic_ = true;
  type(proc::FailType::Error);
  value(v);
}

void Fail::set_badarg(proc::Heap* heap, Term v)
{
  clear();
  set(FailType::Error, term::make_tuple(heap, {atom::BADARG, v}));
}

void Fail::set_badarg()
{
  clear();
  set(FailType::Error, atom::BADARG);
}

} // ns proc
} // ns gluon
