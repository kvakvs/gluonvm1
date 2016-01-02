#include "term.h"
#include "binary.h"
#include "code_server.h"
#include "fun.h"
#include "heap.h"
#include "heap.h"
#include "module.h"  // for Export class
#include "predef_atoms.h"
#include "term_helpers.h"
#include "vm.h"

#include <cstring>

namespace gluon {

Word term::g_zero_sized_tuple = 0;

#if FEATURE_MAPS
Word term::g_zero_sized_map = term_tag::BoxedMap::create_subtag(0);
#endif
#define cSpecialTermColor cYellow cUnderline

template <>
Term term::ConsAspect<Term>::allocate_cons(proc::Heap* heap,
                                           Term head,
                                           Term tail) {
  Term* d = (Term*)heap->allocate<Word>(layout::Cons::box_word_size);
  layout::Cons::head(d) = head;
  layout::Cons::tail(d) = tail;
  return make_cons(d);
}

template <>
bool term::ConsAspect<Term>::is_cons_printable() const {
  Term item = *self();
  while (item.is_cons()) {
    Term tmp;
    item.cons_head_tail(tmp, item);
    if (!is_cons_printable_element(tmp)) {
      return false;
    }
  }
  return item.is_nil();
}

template <>
Str term::AtomAspect<Term>::atom_str(const VM& vm) const {
  return vm.find_atom(*self());
}

#if G_DEBUG
void Term::print(const VM& vm) const {
  if (value() == 0) {
    libc::fmt("NOT_A_TERM");
    return;
  }
  if (is_cons()) {
    if (is_cons_printable()) {
      // list is printable - print quotes and every character except tail
      libc::fmt("\"");
      Word c = (Uint8)cons_head().small_word();
      if (does_char_require_quoting(c)) {
        libc::fmt("\\");
      }
      libc::fmt("%c", (Uint8)c);
      Term item = cons_tail();
      while (item.is_cons()) {
        c = item.cons_head().small_word();
        if (does_char_require_quoting(c)) {
          libc::fmt("\\");
        }
        libc::fmt("%c", (Uint8)c);
        item = item.cons_tail();
      }
      libc::fmt("\"");
    } else {
      // not printable - dump terms and tail
      libc::fmt("[");
      cons_head().print(vm);
      Term item = cons_tail();
      while (item.is_cons()) {
        libc::fmt(",");
        item.cons_head().print(vm);
        item = item.cons_tail();
      }
      if (!item.is_nil()) {
        libc::fmt("|");
        item.print(vm);
      }
      libc::fmt("]");
    }
  } else if (is_tuple()) {
    auto arity = tuple_get_arity();
    libc::fmt("{");
    for (Word n = 0; n < arity; ++n) {
      tuple_get_element(n).print(vm);
      if (n < arity - 1) {
        libc::fmt(",");
      }
    }
    libc::fmt("}");
  } else if (is_boxed()) {
    //
    // ------ BOXED ------
    //
    ContinuationPointer maybe_cp(value());
    if (maybe_cp.check()) {
      libc::fmt(cSpecialTermColor "#CP<");
      vm.codeserver().print_mfa(maybe_cp.untag());
      libc::fmt(">" cRst);
      return;
    }
    auto p = boxed_get_ptr<Word>();
    if (is_boxed_fun()) {
      libc::fmt(cSpecialTermColor "#Fun<");
      auto bf = boxed_get_ptr<BoxedFun>();
      vm.codeserver().print_mfa(bf->fun_entry->code);
      libc::fmt(">" cRst);
      return;
    }
    if (is_boxed_export()) {
      libc::fmt(cSpecialTermColor "#ExportedFun<");
      auto ex = boxed_get_ptr<Export>();
      ex->mfa.print(vm);
      libc::fmt(";");
      if (ex->is_bif()) {
        libc::fmt("bif");
      } else {
        vm.codeserver().print_mfa(ex->code());
      }
      libc::fmt(">" cRst);
      return;
    }
    if (PointerKnowledge::is_userspace_pointer(p)) {
      libc::fmt(cSpecialTermColor "#Box<Tag=" FMT_UWORD ";", boxed_get_subtag());
      vm.codeserver().print_mfa(boxed_get_codeptr());
      libc::fmt(">" cRst);
    } else {
      libc::fmt(cSpecialTermColor "#Box<%p>" cRst, p);
    }
    //
    //
    // ------ end boxed ------
  } else if (is_nil()) {
    libc::fmt("[]");
  } else if (is_nonvalue()) {
    libc::fmt(cSpecialTermColor "NON_VALUE" cRst);
  } else if (is_atom()) {
    libc::fmt("'%s'", atom_str(vm).c_str());
  } else if (is_small()) {
    libc::fmt(FMT_SWORD, small_sword());
  } else if (is_catch()) {
    libc::fmt(cSpecialTermColor "#Catch(" FMT_0xHEX ")" cRst, catch_val());
  } else if (is_short_pid()) {
    libc::fmt("#Pid<" FMT_0xHEX ">", short_pid_get_value());
  } else if (is_regx()) {
    libc::fmt("X[" FMT_UWORD "]", regx_get_value());
  }
#if FEATURE_FLOAT
  else if (is_regfp()) {
    Std::fmt("FP[" FMT_UWORD "]", regfp_get_value());
  }
#endif
  else if (is_regy()) {
    libc::fmt("Y[" FMT_UWORD "]", regy_get_value());
  } else {
    libc::fmt("UNKNOWN(" FMT_0xHEX ")", value());
  }
}

void Term::println(const VM& vm) const {
  print(vm);
  libc::puts();
}

template <>
Term term::BinaryAspect<Term>::make_binary(VM& vm, proc::Heap* h, Word bytes) {
  // This many bytes fits boxed subtag value. Going larger means storing size
  // elsewhere or losing significant bit from the size
  G_ASSERT(bytes < term_tag::boxed_max_subtag_val);

  if (bytes <= bin::heapbin_limit) {
    Word* box = h->allocate<Word>(layout::ProcBin::box_size(bytes));
    layout::ProcBin::set_byte_size(box, bytes);
    return Term(term_tag::BoxedProcBin::create_from_ptr<Word>(box));
  } else {
    // Large bin, with boxed refcount and pointer
    erts::Heap* binheap = vm.get_heap(VM::HeapType::LargeBinary);
    layout::HeapbinBox* box =
        binheap->allocate<layout::HeapbinBox>(layout::HeapBin::box_size(bytes));
    box->set_byte_size(bytes);
    box->set_refcount(1);
    return Term(term_tag::BoxedHeapBin::create_from_ptr(box));
  }
}

void MFArity::println(const VM& vm) {
  print(vm);
  libc::puts();
}

void MFArity::print(const VM& vm) {
  mod.print(vm);
  libc::fmt(":");
  fun.print(vm);
  libc::fmt("/" FMT_UWORD, arity);
}

#endif  // DEBUG

}  // ns gluon
