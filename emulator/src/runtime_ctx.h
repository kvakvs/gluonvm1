#pragma once

#include "term.h"

namespace gluon {
namespace erts {

// Set of stuff we take from Process struct to keep running, this will be saved
// by loop runner on context switch or loop end
class RuntimeContextFields {
 private:
  CodePointer ip_;
  // continuation, works like return address for a single call. If more nested
  // calls are done, cp is saved to stack
  CodePointer cp_;

 public:
  void set_ip(CodePointer ip) {
    G_ASSERT(PointerKnowledge::is_userspace_pointer(ip.value()));
    G_ASSERT(ip.is_not_null());
    ip_ = ip;
  }
  CodePointer ip() const { return ip_; }
  Word ip(Word index) const { return ip_[index]; }
  void step_ip(SWord offset) { ip_ += offset; }
  void inc_ip() { ip_++; }

  void set_cp(CodePointer cp) {
    // some sane minimum for a pointer or nullptr
    G_ASSERT(PointerKnowledge::is_userspace_pointer(cp.value()));
    cp_ = cp;
  }
  CodePointer cp() const { return cp_; }

  Word live = 0;  // saved registers count

  // TODO: maybe cache r0 in a local variable in vm loop?
  Term regs_[erts::max_regs];

#if FEATURE_FLOAT
private:
  Float fp_regs_[vm::MAX_FP_REGS];
public:
  Float fp(Word i) const { return fp_regs[i]; }
  void set_fp(Word i, Float v) { fp_regs[i] = v; }
#else
  Float fp(Word) const { throw err::FeatureMissing("FLOAT"); }
  void set_fp(Word, Float) { throw err::FeatureMissing("FLOAT"); }
#endif

  void print_regs(const VM& vm) const {
    if (debug_mode) {
      for (Word r = 0; r < live; ++r) {
        Std::fmt("x[%zu]=", r);
        regs_[r].println(vm);
      }
    }
  }
};

class RuntimeContext : public RuntimeContextFields {
#if G_DEBUG
 private:
  enum class ContextBelongsTo{
      VmLoop,          // belongs to VM loop, do not modify ctx fields now
      ProcessPartial,  // lightly swapped out (only ip/cp)
      Process,         // fully swapped out with registers etc
  };

  // Extra debug-time check to see if ctx belongs to VM or is swapped out
  ContextBelongsTo belongs_ = ContextBelongsTo::Process;

 public:
  void assert_swapped_out() { G_ASSERT(belongs_ == ContextBelongsTo::Process); }
  void assert_swapped_out_partial() {
    G_ASSERT(belongs_ == ContextBelongsTo::ProcessPartial ||
             belongs_ == ContextBelongsTo::Process);
  }
  void assert_swapped_in() { G_ASSERT(belongs_ == ContextBelongsTo::VmLoop); }
  void swapped_out() {
    assert_swapped_in();
    belongs_ = ContextBelongsTo::Process;
  }
  void swapped_out_partial() {
    assert_swapped_in();
    belongs_ = ContextBelongsTo::ProcessPartial;
  }
  void swapped_in() {
    assert_swapped_out_partial();
    belongs_ = ContextBelongsTo::VmLoop;
  }
#else
 public:
  void assert_swapped_out() {}
  void assert_swapped_out_partial() {}
  void assert_swapped_in() {}
  void swapped_out() {}
  void swapped_out_partial() {}
  void swapped_in() {}
#endif
 public:
  // Entry arguments for apply
  constexpr static Word num_arg_regs = 6;
  Term arg_regs_[num_arg_regs];
};

}  // ns erts
}  // ns gluon
