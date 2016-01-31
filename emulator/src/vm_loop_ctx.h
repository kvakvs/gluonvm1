#pragma once

#include "bif/bif_misc.h"
#include "code_server.h"
#include "fun.h"
#include "genop.h"
#include "heap.h"
#include "module.h"
#include "predef_atoms.h"
#include "process.h"
#include "term_helpers.h"
#include "vm.h"

#include <cstring>

namespace gluon {
namespace impl {

enum class WantSchedule {
    // decided to continue current process
    KeepGoing,
    // we want scheduler to enqueue current process and take next
    NextProcess,
    // process error happened, proc is already swapped out - schedule next
    Error
};

enum class CheckBifError { None, ErrorOccured };

//
// VM execution context, inherited from process context
// Used to run current process by vm loop, also holds some extra local variables
//
class VMRuntimeContext : public erts::RuntimeContextFields {
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
            // ASSERT that we're standing on first instruction of fun after
            // fun_info
            // ip[-4] == fun_info
            // TODO: remove live from all call opcodes and uncomment this
            // live = ip[-1];
            libc::fmt(tMagenta("out of reductions\n"));
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
        std::memcpy(regs_, proc_ctx.regs_, sizeof(Term) * live);

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
        std::memcpy(proc_ctx.regs_, regs_, sizeof(Term) * live);
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

    void move(Term val, Term dst) {
        if (dst.is_regx()) {
            Word x = dst.regx_get_value();
            G_ASSERT(x < sizeof(regs_));
            regs_[x] = val;
        } else if (dst.is_regy()) {
            stack().set_y(dst.regy_get_value(), val.value());
        } else if (feature_float) {
            if (dst.is_regfp()) {
                regs_[dst.regx_get_value()] = val;
            }
        } else {
            throw err::Process("move(_,dst): bad dst");
        }
    }

    // Jumps to location pointed with {extfunc, Mod, Fun, Arity} in BEAM ASM,
    // depending on encoding decision: now it would be a {M,F,Arity} tuple in
    // literals table - so fetch MFA elements, resolve address and jump
    // RETURNS: false if process has to be scheduled out (error occured)
    bool jump_ext(Process* proc, Term mfa_box) {
        G_ASSERT(mfa_box.is_boxed());
        MFArity* mfa = mfa_box.boxed_get_ptr<MFArity>();

        if (debug_mode) {
            libc::fmt(tMagenta("ctx.jump_ext") " -> ");
            mfa->println(vm_);
        }

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
            raise(proc, atom::ERROR, atom::UNDEF);
            return false;
        }

        // check for bif, a nonvalue result with error flag set to undef means
        // that
        // this was not a bif
        return exp->is_bif() ? jump_ext_bif(proc, mfa, exp->bif_fn())
                             : jump_far(proc, mod, exp->code());
    }

    // Finish jump_ext by jumping to a BIF
    // RETURNS: false if process has to be scheduled out (error occured)
    bool jump_ext_bif(Process* proc, MFArity* mfa, void* fn) {
        // Swap out because BIF may decide to modify ip/cp
        swap_out_partial(proc);
        Term result = vm_.apply_bif(proc, mfa->arity, fn, regs_);
        swap_in_partial(proc);

        if (result.is_nonvalue()) {
            if (proc->is_failed()) {
                // a real error happened
                // Term reason = proc->fail_value();
                // proc->fail_clear();
                // return raise(proc, atom::ERROR, reason);
                swap_out(proc);
                proc->handle_error();
                return false;
            }
            // if it was undef - do nothing, it wasn't a bif - we just continue
        } else {
            // simulate real call but return bif result instead
            regs_[0] = result;
            G_ASSERT(cp().is_not_null());
            set_ip(cp());
            set_cp(CodePointer());
        }
        return true;
    }

    // Jumps between modules updating base and mod fields
    bool jump_far(Process* /*proc*/, Module* /*m*/, CodePointer new_ip) {
        set_ip(new_ip);
        return true;
    }

    void jump(Process*, CodePointer ip) { set_ip(ip); }
    void jump(Process*, ContinuationPointer ip) { set_ip(ip.untag()); }

    // Creates an error of type:reason (for example error:badmatch) and
    // processes
    // actions required to handle it
    void raise(Process* proc, Term type, Term reason) {
        proc->fail_clear();
        proc->fail_set(proc::Fail::to_fail_type(type), reason);
        swap_out(proc);
        return proc->handle_error();
    }

    void push_cp() {
        stack().push(ContinuationPointer::make_cp(cp()).value());
        set_cp(CodePointer());
    }

    void pop_cp() {
        ContinuationPointer cp0(stack().pop());
        // auto cp0 = p.boxed_get_ptr_unchecked<Word>();
        set_cp(cp0.untag());
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
        if (debug_mode) {
            libc::fmt("(");
            for (Word i = 0; i < arity; ++i) {
                Term value(ip(i));
                value.print(vm_);
                if (value.is_regx() || value.is_regy()) {
                    resolve_immed(value);
                    libc::fmt("=");
                    value.print(vm_);
                }
                if (i < arity - 1) {
                    libc::fmt(";");
                }
            }  // for
            libc::fmt(")\n");
        }  // if debug
    }

    CheckBifError check_bif_error(Process* p) {
        if (p->is_not_failed()) {
            return CheckBifError::None;
        }
        p->handle_error();
        return CheckBifError::ErrorOccured;
    }

    //void handle_error(Process* p);

    // If value stored in var is immediate and is a special value referring
    // register or stack cell, it gets replaced with value stored in that
    // register
    // or stack cell
    void deref(Term& var) {
        if (var.is_immed()) {
            resolve_immed(var);
        }
    }

    // For special immed1 types (register and stack ref) read actual value
    void resolve_immed(Term& i) const {
        if (i.is_regx()) {
            i = regs_[i.regx_get_value()];
        } else if (i.is_regy()) {
            i = Term(stack().get_y(i.regy_get_value()));
        } else if (feature_float && i.is_regfp()) {
            // i = fp(i.regfp_get_value());
            throw err::FeatureMissing("FLOAT");
        }
    }
};

// A templated opcode handler for bif0..3, behaves slightly differently for bif0
// because there is no Fail label and no jumping to fail label on error. Bif0
// cannot fail.
template <Word NumArgs>
WantSchedule opcode_bif(Process* proc, VMRuntimeContext& ctx) {
    // bif0 import_index Arg1..ArgN Dst
    // bif1..3 Fail import_index Arg1..ArgN Dst
    // Offset for argument. For bif0 there is no Fail label, bif0 cannot fail
    const Word mfa_offset = (NumArgs == 0) ? 0 : 1;

    Term boxed_mfa(ctx.ip(mfa_offset));
    Term arg[NumArgs > 0 ? NumArgs : 1];
    for (Word i = 0; i < NumArgs; ++i) {
        arg[i] = Term(ctx.ip(i + mfa_offset + 1));
        ctx.deref(arg[i]);
    }
    Term result_dst(ctx.ip(NumArgs + mfa_offset + 1));

    MFArity* mfa = boxed_mfa.boxed_get_ptr<MFArity>();
    using BifFn = typename SelectBifFn<NumArgs>::Type;
    auto fun_ptr = (BifFn)ctx.vm_.find_bif(*mfa);
    if (debug_mode) {
        if (!fun_ptr) {
            libc::fmt(tRed("not found bif: "));
        } else {
            libc::fmt(tMagenta("call bif: "));
        }
        mfa->println(ctx.vm_);
    }
    G_ASSERT(fun_ptr);

    // --- How to do a bif call ---
    // Following few lines should happen at each bif call
    // Swapout
    // Assert is not exiting
    // ... bif call goes ...
    // TODO: assert !proc is exiting or is non_value returned (error state)
    // Swapin
    // call handle_error
    //-------------------

    // Run the BIF here
    Term result = SelectBifFn<NumArgs>::apply(fun_ptr, proc, arg);

    // If error occured and bif1..3 we jump to fail label OR throw
    // If error occured for bif0 we throw always
    if (NumArgs > 0) {  // bif0 cannot fail so we have no fail label
        ContinuationPointer fail_label(ctx.ip(0));
        if (fail_label.check()) {
            ctx.jump(proc, fail_label.untag());
            return WantSchedule::KeepGoing;
        }
    }  // end if bif1-3

    if (ctx.check_bif_error(proc) == CheckBifError::ErrorOccured) {
        return WantSchedule::NextProcess;
    }
    ctx.move(result, result_dst);
    ctx.step_ip(2 + NumArgs);
    return ctx.consume_reduction(proc);
}

}  // ns impl
}  // ns gluon
