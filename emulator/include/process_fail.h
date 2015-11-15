#pragma once

#include "term.h"

namespace gluon {
namespace proc {

enum class FailType {
  None,
  Exit,   // Process termination - not an error
  Error,  // Error (adds stacktrace; will be logged)
  Throw   // Nonlocal return (turns into a 'nocatch' error if not caught)
};

//
// Holds process error state and provides accessors to it
// Does not have knowledge about owning process
//
class Fail {
private:
  bool panic_: 1;  // ignore catches
  bool thrown_: 1; // nonlocal return
  bool log_: 1;    // write to logger on termination
  //bool native: 1; // occurred in native code (not impl)
  //bool save_trace: 1; // save stack trace in internal form (not impl)
  bool arg_list_: 1; // has arglist for top of trace
  FailType type_: 2;
  Term value_ = the_non_value;

public:
  Fail(): panic_(false), thrown_(false), log_(false), arg_list_(false),
    type_(FailType::None) {}

  FailType type() const { return type_; }
  void type(FailType ft) { type_ = ft; }

  Term value() const { return value_; }
  void value(Term v) { value_ = v; }

  bool is_failed() const { return type_ != FailType::None; }
  bool is_not_failed() const { return type_ == FailType::None; }

  void clear() {
    type_ = FailType::None;
    value_ = the_non_value;
    panic_ = thrown_ = log_ = arg_list_ = false;
  }

  // Sets error state
  // type is atom error|throw|exit, unknown will become error
  void set(FailType ft, Term reason);
  static FailType to_fail_type(Term type);

  // Fills fields for some common error reasons
  void set_normal_exit(); // exit
  void set_internal_error(Term v); // error+panic
  void set_badarg(proc::Heap* heap, Term v);
  void set_badarg();
  void set_badarith();
  void set_badmatch();
  void set_fun_clause();
  void set_case_clause();
  void set_if_clause();
  void set_undef();
  void set_badfun();
  void set_badarity();
  void set_timeout_value();
  void set_noproc();
  void set_notalive();
  void set_system_limit();
  void set_try_clause();
  void set_not_supported();
  void set_badmap();
  void set_badkey();
};

} // ns proc
} // ns gluon
