#include "vm_ctx.h"

namespace gluon {
namespace impl {

CheckBifError VMRuntimeContext::check_bif_error(Process *p) {
  if (p->is_not_failed()) {
    return CheckBifError::None;
  }
  // --- How to do a bif call ---
  // Following few lines should happen at each bif call
  // Swapout
  // Assert is not exiting
  // ... bif call goes ...

  // TODO: assert !proc is exiting or is non_value returned (error state)
  // Swapin
  // call handle_error

  //Term reason = p->fail_.value();
  //p->fail_.clear();
  //this->raise(p, atom::ERROR, reason);
  this->handle_error(p);
  return CheckBifError::ErrorOccured;
}

// Maybe this belongs to Process?
void VMRuntimeContext::handle_error(Process *p) {
  // --- in handle error do
  // If fail_.arg_list? (parse tuple with error value and args)
  // If save trace flag set? Save stacktrace
  // If throw and catch level <= 0: convert Value into {nocatch, Value}
  // call expand_error_value (OTP)
  // If catches > 0 || traced && !panic in reason
  //    fill regs: [nonvalue, exception_tag, Value, p->ftrace]
  //    new_ip=find next catch, cp=0, return new ip
  //    if still catches>0 return erl_exit catch not found
  // else terminate proc with Value
 throw err::TODO("handle error");
}

CodePointer VMRuntimeContext::find_next_catch(CodePointer cp) {
  /* For stack end to stack start do {
        if ptr=start return not found
        if !ptr.is_cp || (*ptr.value() not in return_[trace,to_trace,time_trace])
          && proc->cp) {
          cpp = proc->cp
          if cpp == beam_exc_trace...: ptr += 2
          elif cpp == beam_return_trace...: ptr += 2
          ... return_time_trace: ptr++
          ... return_to_trace: have_return_to_trace=true
        }
        while (ptr < stack start) {
          if ptr.is_catch {
            if active_catches (that is p->catch_count > 0) { goto found }
          } else if ptr.is_cp {
            prev = ptr
            if prev.cp_val == return_trace {
              ... magic
              ptr += 2
            } else is == return_to_Trace {
              ... magic
              ptr += 2
            } else is == return_time_trace {
              ... magic
              ptr++
            } else {
              if havereturn_to_trace {
                set it to false
                return_to_trace_ptr = ptr
              } else return_to_trace_ptr = nullptr
            }
          } else ptr++
        }
        return not found
        found:
          assert ptr still in stack
          proc->stop = prev
          if is_traced && return_to_trace_ptr {
            erts trace return to
          }
          else return ptr.catch_value
     }*/
  throw err::TODO("find next catch");
}

}  // ns impl
}  // ns gluon
