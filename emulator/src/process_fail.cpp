#include "process_fail.h"
#include "predef_atoms.h"

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

} // ns proc
} // ns gluon
