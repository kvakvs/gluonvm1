#include "g_term.h"
#include "g_vm.h"
#include "g_heap.h"
#include "g_code_server.h"
#include "g_fun.h"
#include "g_module.h"  // for Export class
#include "g_term_helpers.h"
#include "g_heap.h"
#include "g_binary.h"
#include "g_predef_atoms.h"

#include <cstring>

namespace gluon {

Word term::g_zero_sized_tuple = 0;

#if FEATURE_MAPS
Word term::g_zero_sized_map = term_tag::BoxedMap::create_subtag(0);
#endif

Term Term::allocate_cons(proc::Heap* heap, Term head, Term tail) {
  Term* d = (Term*)heap->allocate<Word>(layout::CONS::box_word_size);
  layout::CONS::head(d) = head;
  layout::CONS::tail(d) = tail;
  return make_cons(d);
}

bool Term::is_cons_printable() const {
  Term item = *this;
  while (item.is_cons()) {
    Term tmp;
    item.cons_head_tail(tmp, item);
    if (!is_cons_printable_element(tmp)) {
      return false;
    }
  }
  return item.is_nil();
}

Word Term::cons_to_array(Term* arr, Word limit) {
  if (is_nil()) {
    return 0;
  }

  *arr = cons_head();
  arr++;

  Term i = cons_tail();
  Word n = 1;

  while (n < limit && i.is_cons()) {
    // splits i into head and tail, tail is assigned to i again
    i.cons_head_tail(*arr, i);
    ++arr;
    ++n;
  }
  return n;
}

bool Term::is_cons_printable_element(Term el) {
  if (!el.is_small())
    return false;
  Word c = el.small_word();
  return (c >= ' ' && c <= 127);
}

Str Term::atom_str(const VM& vm) const {
  return vm.find_atom(*this);
}

#if G_DEBUG
void Term::print(const VM& vm) const {
  if (value_ == 0) {
    Std::fmt("NOT_A_TERM");
    return;
  }
  if (is_cons()) {
    if (is_cons_printable()) {
      // list is printable - print quotes and every character except tail
      Std::fmt("\"");
      Word c = (Uint8)cons_head().small_word();
      if (does_char_require_quoting(c)) {
        Std::fmt("\\");
      }
      Std::fmt("%c", (Uint8)c);
      Term item = cons_tail();
      while (item.is_cons()) {
        c = item.cons_head().small_word();
        if (does_char_require_quoting(c)) {
          Std::fmt("\\");
        }
        Std::fmt("%c", (Uint8)c);
        item = item.cons_tail();
      }
      Std::fmt("\"");
    } else {
      // not printable - dump terms and tail
      Std::fmt("[");
      cons_head().print(vm);
      Term item = cons_tail();
      while (item.is_cons()) {
        Std::fmt(",");
        item.cons_head().print(vm);
        item = item.cons_tail();
      }
      if (!item.is_nil()) {
        Std::fmt("|");
        item.print(vm);
      }
      Std::fmt("]");
    }
  } else if (is_tuple()) {
    auto arity = tuple_get_arity();
    Std::fmt("{");
    for (Word n = 0; n < arity; ++n) {
      tuple_get_element(n).print(vm);
      if (n < arity - 1) {
        Std::fmt(",");
      }
    }
    Std::fmt("}");
  } else if (is_boxed()) {
    auto p = boxed_get_ptr<Word>();
    if (term_tag::is_cp<Word>(p)) {
      Word* cp = term_tag::untag_cp<Word>(p);
      Std::fmt("#CP<");
      vm.codeserver().print_mfa(cp);
      Std::fmt(">");
      return;
    }
    if (is_boxed_fun()) {
      Std::fmt("#Fun<");
      auto bf = boxed_get_ptr<BoxedFun>();
      //      if ((Word)(bf->fun_entry) < 0x1000) {
      //        Std::fmt(FMT_0xHEX, (Word)bf->fun_entry);
      //      } else {
      vm.codeserver().print_mfa(bf->fun_entry->code);
      //      }
      Std::fmt(">");
      return;
    }
    if (is_boxed_export()) {
      Std::fmt("#ExportedFun<");
      auto ex = boxed_get_ptr<Export>();
      ex->mfa.print(vm);
      Std::fmt(";");
      if (ex->is_bif()) {
        Std::fmt("bif");
      } else {
        vm.codeserver().print_mfa(ex->code());
      }
      Std::fmt(">");
      return;
    }
    Std::fmt("#Box<Tag=" FMT_UWORD ";", boxed_get_subtag());
    vm.codeserver().print_mfa(boxed_get_ptr<Word>());
    Std::fmt(">");
  } else if (is_nil()) {
    Std::fmt("[]");
  } else if (is_non_value()) {
    Std::fmt("NON_VALUE");
  } else if (is_atom()) {
    Std::fmt("'%s'", atom_str(vm).c_str());
  } else if (is_small()) {
    Std::fmt(FMT_SWORD, small_sword());
  } else if (is_catch()) {
    Std::fmt("#Catch(" FMT_0xHEX ")", catch_val());
  } else if (is_short_pid()) {
    Std::fmt("#Pid<" FMT_0xHEX ">", short_pid_get_value());
  } else if (is_regx()) {
    Std::fmt("X[" FMT_UWORD "]", regx_get_value());
  }
#if FEATURE_FLOAT
  else if (is_regfp()) {
    Std::fmt("FP[" FMT_UWORD "]", regfp_get_value());
  }
#endif
  else if (is_regy()) {
    Std::fmt("Y[" FMT_UWORD "]", regy_get_value());
  } else {
    Std::fmt("UNKNOWN(" FMT_0xHEX ")", value_);
  }
}

void Term::println(const VM& vm) const {
  print(vm);
  Std::puts();
}

Term Term::make_binary(VM& vm, proc::Heap* h, Word bytes) {
  // This many bytes fits boxed subtag value. Going larger means storing size
  // elsewhere or losing significant bit from the size
  G_ASSERT(bytes < term_tag::boxed_max_subtag_val);

  if (bytes <= bin::heapbin_limit) {
    Word* box = h->allocate<Word>(layout::PROC_BIN::box_size(bytes));
    layout::PROC_BIN::set_byte_size(box, bytes);
    return Term(term_tag::BoxedProcBin::create_from_ptr<Word>(box));
  } else {
    // Large bin, with boxed refcount and pointer
    erts::Heap* binheap = vm.get_heap(VM::HEAP_LARGE_BINARY);
    layout::HeapbinBox* box = binheap->allocate<layout::HeapbinBox>(
        layout::HEAP_BIN::box_size(bytes));
    box->set_byte_size(bytes);
    box->set_refcount(1);
    return Term(term_tag::BoxedHeapBin::create_from_ptr(box));
  }
}

void MFArity::println(const VM& vm) {
  print(vm);
  Std::puts();
}

void MFArity::print(const VM& vm) {
  mod.print(vm);
  Std::fmt(":");
  fun.print(vm);
  Std::fmt("/" FMT_UWORD, arity);
}

Word layout::PROC_BIN::box_size(Word bytes) {
  return calculate_word_size(bytes) + box_extra_words;
}

Word layout::HEAP_BIN::box_size(Word bytes) {
  return calculate_word_size(bytes) + farheap_extra_words;
}

#endif  // DEBUG

}  // ns gluon
