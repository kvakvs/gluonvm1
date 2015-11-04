#pragma once

#include "process.h"
#include "module.h"
#include "code_server.h"
#include "bif/g_bif_misc.h"
#include "genop.h"
#include "predef_atoms.h"
#include "fun.h"
#include "heap.h"
#include "vm.h"
#include "term_helpers.h"

#include <cstring>

namespace gluon {
namespace impl {

enum class WantSchedule {
  NextProcess,  // we want scheduler to enqueue current process and take next
  KeepGoing     // decided to continue current process
};

//
// VM execution context, inherited from process context
// Used to run current process by vm loop, also holds some extra local variables
//
class VMRuntimeContext: public erts::RuntimeContextFields {
public:
  VM& vm_;
  proc::Heap* heap_;
  SWord reds_ = 0;

  VMRuntimeContext(VM& vm) : vm_(vm) {}

  proc::Stack& stack() { return heap_->stack_; }
  const proc::Stack& stack() const { return heap_->stack_; }

  void println() {
    //#if G_DEBUG
    //#endif
  }

  // returns false if process should yield (for call ops for example)
  WantSchedule consume_reduction(Process* p) {
    reds_--;
    if (G_UNLIKELY(reds_ <= 0)) {
      // ASSERT that we're standing on first instruction of fun after fun_info
      // ip[-4] == fun_info
      // TODO: remove live from all call opcodes and uncomment this
      // live = ip[-1];
      swap_out_partial(p);
      return WantSchedule::NextProcess;
    }
    return WantSchedule::KeepGoing;
  }

  void swap_in(Process* proc) {
    erts::RuntimeContext& proc_ctx = proc->get_runtime_ctx();
    proc_ctx.swapped_in();
    set_ip(proc_ctx.ip());
    set_cp(proc_ctx.cp());
    live = proc_ctx.live;
    std::memcpy(regs, proc_ctx.regs, sizeof(Term) * live);

    heap_ = proc->get_heap();
    // TODO: fp_regs
    // TODO: update heap top
    reds_ = erts::reductions_per_slice;
  }

  void swap_out(Process* proc) {
    erts::RuntimeContext& proc_ctx = proc->get_runtime_ctx();
    proc_ctx.swapped_out();
    proc_ctx.set_ip(ip());
    proc_ctx.set_cp(cp());
    proc_ctx.live = live;
    std::memcpy(proc_ctx.regs, regs, sizeof(Term) * live);
    // TODO: fp_regs
    // TODO: update heap top
    heap_ = nullptr;  // can't use once swapped out
  }

  // TODO: swap_out: save r0, stack top and heap top
  void swap_out_partial(Process* proc) {
    erts::RuntimeContext& proc_ctx = proc->get_runtime_ctx();
    proc_ctx.swapped_out_partial();
    proc_ctx.set_ip(ip());
    proc_ctx.set_cp(cp());
  }

  void swap_in_partial(Process* proc) {
    erts::RuntimeContext& proc_ctx = proc->get_runtime_ctx();
    proc_ctx.swapped_in();
    set_ip(proc_ctx.ip());
    set_cp(proc_ctx.cp());
  }

  // For special immed1 types (register and stack ref) convert them to their
  // values
  // TODO: move to context in g_vm_impl.h
  void resolve_immed(Term& i) const {
    if (i.is_regx()) {
      i = regs[i.regx_get_value()];
    } else if (i.is_regy()) {
      i = Term(stack().get_y(i.regy_get_value()));
    }
#if FEATURE_FLOAT
    else if (i.is_regfp()) {
      i = fp_regs[i.regfp_get_value()];
    }
#endif
  }

  void move(Term val, Term dst) {
//#if G_DEBUG
//    Std::fmt(tMagenta("ctx.move "));
//    val.print(vm_);
//    Std::fmt(" -> ");
//    dst.println(vm_);
//#endif
    if (dst.is_regx()) {
      Word x = dst.regx_get_value();
      G_ASSERT(x < sizeof(regs));
      regs[x] = val;
    } else if (dst.is_regy()) {
      stack().set_y(dst.regy_get_value(), val.as_word());
    } else if (feature_float) {
      if (dst.is_regfp()) {
        regs[dst.regx_get_value()] = val;
      }
    } else {
      throw err::Process("bad move dst");
    }
  }

  // Jumps to location pointed with {extfunc, Mod, Fun, Arity} in BEAM ASM,
  // depending on encoding decision: now it would be a {M,F,Arity} tuple in
  // literals table - so fetch MFA elements, resolve address and jump
  void jump_ext(Process* proc, Term mfa_box) {
    G_ASSERT(mfa_box.is_boxed());
    MFArity* mfa = mfa_box.boxed_get_ptr<MFArity>();
    Std::fmt(tMagenta("ctx.jump_ext") " -> ");
    mfa->println(vm_);

    // Quick check if it accidentally is a BIF
    if (mfa->mod == atom::ERLANG) {
      auto maybe_bif = vm_.find_bif(*mfa);
      if (maybe_bif) {
        return jump_ext_bif(proc, mfa, maybe_bif);
      }
    }

    Module* mod = nullptr;
    Export* exp = vm_.codeserver().find_mfa(*mfa, &mod);
    if (!exp) {
      return raise(proc, atom::ERROR, atom::UNDEF);
    }

    // check for bif, a nonvalue result with error flag set to undef means that
    // this was not a bif
    return exp->is_bif() ? jump_ext_bif(proc, mfa, exp->bif_fn())
                         : jump_far(proc, mod, exp->code());
  }

  // Finish jump_ext by jumping to a BIF
  void jump_ext_bif(Process* proc, MFArity* mfa, void* fn) {
    // Swap out because BIF may decide to modify ip/cp
    swap_out_partial(proc);
    Term result = vm_.apply_bif(proc, mfa->arity, fn, regs);
    swap_in_partial(proc);

    if (result.is_non_value()) {
      if (proc->bif_err_reason_ != atom::UNDEF) {
        // a real error happened
        Term reason = proc->bif_err_reason_;
        proc->bif_err_reason_ = the_non_value;
        return raise(proc, atom::ERROR, reason);
      }
      // if it was undef - do nothing, it wasn't a bif - we just continue
    } else {
      // simulate real call but return bif result instead
      regs[0] = result;
      G_ASSERT(cp());
      set_ip(cp());
      set_cp(nullptr);
      return;
    }
  }

  // Jumps between modules updating base and mod fields
  void jump_far(Process* proc, Module* m, Word* new_ip) {
    //    mod = m;
    // base = proc->get_code_base();
    set_ip(new_ip);
  }

  void jump(Process* proc, Term t) {
    G_ASSERT(t.is_boxed() && term_tag::is_cp(t.boxed_get_ptr<Word>()));
    Word* t_ptr = term_tag::untag_cp(t.boxed_get_ptr<Word>());
    Std::fmt(tMagenta("ctx.jump") " -> " FMT_0xHEX "\n", (Word)t_ptr);
    set_ip(t_ptr);
    // TODO: some meaningful assertion here?
    // G_ASSERT(ip > base);
    // G_ASSERT(ip < proc->m_module->m_code.size() + base);
  }

  // Throws type:reason (for example error:badmatch)
  void raise(Process* proc, Term type, Term reason) {
    swap_out_partial(proc);
    regs[0] = type;
    regs[1] = reason;
    proc->stack_trace_ = the_non_value;
    return exception(proc);
  }

  void exception(Process* proc) {
    if (proc->catch_level_ == 0) {
      // we're not catching anything here
      Std::fmt(tRed("VM EXCEPTION: "));
      regs[0].print(vm_);
      Std::fmt(":");
      regs[1].println(vm_);
      throw err::Process(tRed("Stopping execution here"));
    }
    // unwind stack
  }

  void push_cp() {
    stack().push(Term::make_boxed_cp(cp()).as_word());
    set_cp(nullptr);
  }

  void pop_cp() {
    Term p(stack().pop());
    auto cp0 = p.boxed_get_ptr_unchecked<Word>();
    set_cp(term_tag::untag_cp<Word>(cp0));
  }

  void stack_allocate(Word n) {
    stack().push_n_nils(n);
    push_cp();
  }

  void stack_deallocate(Word n) {
    pop_cp();
    stack().drop_n(n);
  }

  void print_args(Word arity) {
#if G_DEBUG
    Std::fmt("(");
    for (Word i = 0; i < arity; ++i) {
      Term value(ip(i));
      value.print(vm_);
      if (value.is_regx() || value.is_regy()) {
        resolve_immed(value);
        Std::fmt("=");
        value.print(vm_);
      }
      if (i < arity - 1) { Std::fmt(";"); }
    }
    Std::fmt(")\n");
#endif
  }

  bool check_bif_error(Process* p) {
    Term reason = p->bif_err_reason_;
    if (reason.is_non_value()) {
      return false;  // good no error
    }
    p->bif_err_reason_ = the_non_value;
    raise(p, atom::ERROR, reason);
    return true;
  }
};

}  // ns impl
}  // ns gluon
