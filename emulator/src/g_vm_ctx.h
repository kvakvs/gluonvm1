#pragma once

#include "g_process.h"
#include "g_module.h"
#include "g_code_server.h"
#include "bif/g_bif_misc.h"
#include "g_genop.h"
#include "g_predef_atoms.h"
#include "g_fun.h"
#include "g_heap.h"
#include "g_vm.h"
#include "g_term_helpers.h"

#include <cstring>

namespace gluon {
namespace impl {

typedef enum {
  SCHEDULE_NEXT,
  KEEP_GOING
} want_schedule_t;

//
// VM execution context, inherited from process context
// Used to run current process by vm loop, also holds some extra local variables
//
struct vm_runtime_ctx_t: runtime_ctx_t {
  VM          &vm_;
  proc::Heap  *heap_;
  sword_t     reds_ = 0;

  vm_runtime_ctx_t(VM &vm): vm_(vm) {}

  proc::Stack &stack() { return heap_->stack_; }
  const proc::Stack &stack() const { return heap_->stack_; }

  void println() {
//#if G_DEBUG
//#endif
  }

  // returns false if process should yield (for call ops for example)
  want_schedule_t consume_reduction(Process *p) {
    reds_--;
    if (G_UNLIKELY(reds_ <= 0)) {
      // ASSERT that we're standing on first instruction of fun after fun_info
      // ip[-4] == fun_info
      // TODO: remove live from all call opcodes and uncomment this
      // live = ip[-1];
      swap_out_light(p);
      return SCHEDULE_NEXT;
    }
    return KEEP_GOING;
  }

  void load(Process *proc) {
    runtime_ctx_t &proc_ctx = proc->get_runtime_ctx();
    ip   = proc_ctx.ip;
    cp   = proc_ctx.cp;
    live = proc_ctx.live;
    std::memcpy(regs, proc_ctx.regs, sizeof(Term)*live);

    heap_ = proc->get_heap();
    // TODO: fp_regs
    // TODO: update heap top
    reds_ = erts::SLICE_REDUCTIONS;
  }

  void save(Process *proc) {
    runtime_ctx_t &proc_ctx = proc->get_runtime_ctx();
    proc_ctx.ip   = ip;
    proc_ctx.cp   = cp;
    proc_ctx.live = live;
    std::memcpy(proc_ctx.regs, regs, sizeof(Term)*live);
    // TODO: fp_regs
    // TODO: update heap top
    heap_ = nullptr; // can't use once swapped out
  }

  // TODO: swap_out: save r0, stack top and heap top
  inline void swap_out_light(Process *proc) {
    // Make this little lighter
    return save(proc);
  }

  // For special immed1 types (register and stack ref) convert them to their
  // values
  // TODO: move to context in g_vm_impl.h
  void resolve_immed(Term &i) const {
    if (i.is_regx()) {
      i = regs[i.regx_get_value()];
    }
    else if (i.is_regy()) {
      i = Term(stack().get_y(i.regy_get_value()));
    }
#if FEATURE_FLOAT
    else if (i.is_regfp()) {
      i = fp_regs[i.regfp_get_value()];
    }
#endif
  }

  void move(Term val, Term dst) {
#if G_DEBUG
    Std::fmt("ctx.move ");
    val.print(vm_);
    Std::fmt(" -> ");
    dst.println(vm_);
#endif
    if (dst.is_regx()) {
      word_t x = dst.regx_get_value();
      G_ASSERT(x < sizeof(regs));
      regs[x] = val;
    } else
    if (dst.is_regy()) {
      stack().set_y(dst.regy_get_value(), val.as_word());
    } else
#if FEATURE_FLOAT
    if (dst.is_regfp()) {
      regs[dst.regx_get_value()] = val;
    } else
#endif
    {
      G_FAIL("bad move dst")
    }
  }

  // Jumps to location pointed with {extfunc, Mod, Fun, Arity} in BEAM ASM,
  // depending on encoding decision: now it would be a {M,F,Arity} tuple in
  // literals table - so fetch MFA elements, resolve address and jump
  void jump_ext(Process *proc, Term mfa_box) {
    G_ASSERT(mfa_box.is_boxed());
    mfarity_t *mfa = mfa_box.boxed_get_ptr<mfarity_t>();
    Std::fmt("ctx.jump_ext -> ");
    mfa->println(vm_);

    Module *mod = nullptr;
    export_t *exp = vm_.codeserver().find_mfa(*mfa, &mod);
    if (!exp) {
      return raise(proc, atom::ERROR, atom::UNDEF);
    }

    // check for bif, a nonvalue result with error flag set to undef means that
    // this was not a bif
    //void *bif_fn = VM::find_bif(*mfa);
    if (exp->is_bif()) {
      Term result = vm_.apply_bif(proc, mfa->arity, exp->bif_fn(), regs);
      if (result.is_non_value()) {
        if (proc->m_bif_error_reason != atom::UNDEF) {
          // a real error happened
          Term reason = proc->m_bif_error_reason;
          proc->m_bif_error_reason = NONVALUE;
          return raise(proc, atom::ERROR, reason);
        }
        // if it was undef - do nothing, it wasn't a bif - we just continue
      } else {
        // simulate real call but return bif result instead
        regs[0] = result;
        G_ASSERT(cp);
        ip = cp;
        cp = nullptr;
        return;
      }
    }

    /*
    auto find_result = VM::codeserver().find_module(proc, mfa->mod,
                                                 code::LOAD_IF_NOT_FOUND);
    if (find_result.is_error()) {
      G_LOG("ctx.jump_ext: %s\n", find_result.get_error());
      return raise(proc, atom::ERROR, atom::UNDEF);
    }

    Module *mod = find_result.get_result();
    auto find_fn_result = mod->resolve_function(mfa->fun, mfa->arity);
    if (find_fn_result.is_error()) {
      G_LOG("ctx.jump_ext: %s\n", find_fn_result.get_error());
      return raise(proc, atom::ERROR, atom::UNDEF);
    }
    return jump_far(proc, mod, find_fn_result.get_result());
    */
    return jump_far(proc, mod, exp->code());
  }

  // Jumps between modules updating base and mod fields
  void jump_far(Process *proc, Module *m, word_t *new_ip) {
//    mod = m;
    //base = proc->get_code_base();
    ip = new_ip;
  }

  void jump(Process *proc, Term t) {
    G_ASSERT(t.is_boxed() && term_tag::is_cp(t.boxed_get_ptr<word_t>()));
    word_t *t_ptr = term_tag::untag_cp(t.boxed_get_ptr<word_t>());
    Std::fmt("ctx.jump -> " FMT_0xHEX "\n", (word_t)t_ptr);
    ip = t_ptr;
    // TODO: some meaningful assertion here?
    //G_ASSERT(ip > base);
    //G_ASSERT(ip < proc->m_module->m_code.size() + base);
  }

  // Throws type:reason (for example error:badmatch)
  void raise(Process *proc, Term type, Term reason) {
    swap_out_light(proc);
    regs[0] = type;
    regs[1] = reason;
    proc->m_stack_trace = NONVALUE;
    return exception(proc);
  }

  void exception(Process *proc) {
    if (proc->m_catch_level == 0) {
      // we're not catching anything here
      Std::fmt("EXCEPTION: ");
      regs[0].print(vm_);
      Std::fmt(":");
      regs[1].println(vm_);
      G_FAIL("Stopping execution here");
    }
    // unwind stack
/*
    for (word_t i = stack->size()-1; i > 0; --i)
    {
      if (stack[i].is_catch()) {
        //word_t jump_to = (*stack)[i].catch_val();
        do {
          i++;
        } while (!(*stack)[i].is_boxed()
                 || !term_tag::is_cp((*stack)[i].boxed_get_ptr<word_t>()));
        // TODO: trim stack to iter


        //ip = catch_jump(index);
        cp = nullptr;
      }
      // TODO: set process exit reason
      procmo->get_runtime_ctx().live = 0;
      // TODO: schedule next process in queue
    }
*/
  }
  void push_cp() {
    stack().push(Term::make_boxed_cp(cp).as_word());
    cp = nullptr;
  }
  void pop_cp() {
    Term p(stack().pop());
    cp = term_tag::untag_cp<word_t>(p.boxed_get_ptr<word_t>());
  }
  inline void stack_allocate(word_t n) {
    stack().push_n_nils(n);
    push_cp();
  }
  inline void stack_deallocate(word_t n) {
    pop_cp();
    stack().drop_n(n);
  }

  void print_args(word_t arity) {
#if G_DEBUG
    for (word_t i = 0; i < arity; ++i) {
      Term value(ip[i]);
      value.print(vm_);
      if (value.is_regx() || value.is_regy()) {
        resolve_immed(value);
        Std::fmt("=");
        value.print(vm_);
      }
      Std::fmt(";");
    }
    Std::puts();
#endif
  }
  bool check_bif_error(Process *p) {
    Term reason = p->m_bif_error_reason;
    if (reason.is_non_value()) {
      return false; // good no error
    }
    p->m_bif_error_reason = NONVALUE;
    raise(p, atom::ERROR, reason);
    return true;
  }
};

} // ns impl
} // ns gluon
