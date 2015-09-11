#include "g_term.h"
#include "g_vm.h"
#include "g_heap.h"
#include "g_code_server.h"
#include "g_fun.h"
#include "g_module.h" // for export_t class
#include "g_term_helpers.h"
#include "g_heap.h"
#include "g_binary.h"

namespace gluon {

word_t term::g_zero_sized_tuple = 0;

#if FEATURE_MAPS
word_t term::g_zero_sized_map = term_tag::BoxedMap::create_subtag(0);
#endif

Term Term::allocate_cons(proc::Heap *heap, Term head, Term tail) {
  Term *d = (Term *)heap->h_alloc(layout::CONS::BOX_SIZE);
  layout::CONS::head(d) = head;
  layout::CONS::tail(d) = tail;
  return make_cons(d);
}

bool Term::is_cons_printable() const
{
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

word_t Term::cons_to_array(Term *arr, word_t limit)
{
  if (is_nil()) {
    return 0;
  }

  *arr = cons_head();
  arr++;

  Term i = cons_tail();
  word_t n = 1;

  while (n < limit && i.is_cons()) {
    // splits i into head and tail, tail is assigned to i again
    i.cons_head_tail(*arr, i);
    ++arr;
    ++n;
  }
  return n;
}

bool Term::is_cons_printable_element(Term el) {
  if (!el.is_small()) return false;
  word_t c = el.small_get_unsigned();
  return (c >= ' ' && c <= 127);
}

Str Term::atom_str() const
{
  return VM::find_atom(*this);
}

#if G_DEBUG
void Term::print() const
{
  if (m_val == 0) {
    Std::fmt("NOT_A_TERM");
    return;
  }
  if (is_cons()) {
    if (is_cons_printable()) {
      // list is printable - print quotes and every character except tail
      Std::fmt("\"");
      word_t c = (u8_t)cons_head().small_get_unsigned();
      if (does_char_require_quoting(c)) {
        Std::fmt("\\");
      }
      Std::fmt("%c", (u8_t)c);
      Term item = cons_tail();
      while (item.is_cons()) {
        c = item.cons_head().small_get_unsigned();
        if (does_char_require_quoting(c)) {
          Std::fmt("\\");
        }
        Std::fmt("%c", (u8_t)c);
        item = item.cons_tail();
      }
      Std::fmt("\"");
    } else {
      // not printable - dump terms and tail
      Std::fmt("[");
      cons_head().print();
      Term item = cons_tail();
      while (item.is_cons()) {
        Std::fmt(",");
        item.cons_head().print();
        item = item.cons_tail();
      }
      if (!item.is_nil()) {
        Std::fmt("|");
        item.print();
      }
      Std::fmt("]");
    }
  }
  else if (is_tuple()) {
    auto arity = tuple_get_arity();
    Std::fmt("{");
    for (word_t n = 0; n < arity; ++n) {
      tuple_get_element(n).print();
      if (n < arity-1) {
        Std::fmt(",");
      }
    }
    Std::fmt("}");
  }
  else if (is_boxed()) {
    auto p = boxed_get_ptr<word_t>();
    if (term_tag::is_cp<word_t>(p)) {
      word_t *cp = term_tag::untag_cp<word_t>(p);
      Std::fmt("#CP<");
      VM::get_cs()->print_mfa(cp);
      Std::fmt(">");
      return;
    }
    if (is_boxed_fun()) {
      Std::fmt("#Fun<");
      auto bf = boxed_get_ptr<boxed_fun_t>();
//      if ((word_t)(bf->fun_entry) < 0x1000) {
//        Std::fmt(FMT_0xHEX, (word_t)bf->fun_entry);
//      } else {
        VM::get_cs()->print_mfa(bf->fun_entry->code);
//      }
      Std::fmt(">");
      return;
    }
    if (is_boxed_export()) {
      Std::fmt("#ExportedFun<");
      auto ex = boxed_get_ptr<export_t>();
      ex->mfa.print();
      Std::fmt(";");
      if (ex->is_bif()) {
        Std::fmt("bif");
      } else {
        VM::get_cs()->print_mfa(ex->code);
      }
      Std::fmt(">");
      return;
    }    Std::fmt("#Box<Tag=" FMT_UWORD ";", boxed_get_subtag());
    VM::get_cs()->print_mfa(boxed_get_ptr<word_t>());
    Std::fmt(">");
  }
  else if (is_nil()) {
    Std::fmt("[]");
  }
  else if (is_non_value()) {
    Std::fmt("NON_VALUE");
  }
  else if (is_atom()) {
    Std::fmt("'%s'", atom_str().c_str());
  }
  else if (is_small()) {
    Std::fmt(FMT_SWORD, small_get_signed());
  }
  else if (is_catch()) {
    Std::fmt("CATCH(" FMT_0xHEX ")", catch_val());
  }
  else if (is_short_pid()) {
    Std::fmt("#Pid<" FMT_0xHEX ">", short_pid_get_value());
  }
  else if (is_regx()) {
    Std::fmt("X[" FMT_UWORD "]", regx_get_value());
  }
#if FEATURE_FLOAT
  else if (is_regfp()) {
    Std::fmt("FP[" FMT_UWORD "]", regfp_get_value());
  }
#endif
  else if (is_regy()) {
    Std::fmt("Y[" FMT_UWORD "]", regy_get_value());
  }
  else {
    Std::fmt("UNKNOWN(" FMT_0xHEX ")", m_val);
  }
}

void Term::println() const
{
  print();
  puts("");
}

Term Term::make_binary(proc::Heap *h, word_t bytes)
{
  // This many bytes fits boxed subtag value. Going larger means storing size
  // elsewhere or losing significant bit from the size
  G_ASSERT(bytes < term_tag::BOXED_MAX_SUBTAG_VALUE);

  if (bytes <= bin::HEAP_BIN_LIMIT) {
    word_t *box = h->h_alloc(layout::PROC_BIN::box_size(bytes));
    layout::PROC_BIN::set_byte_size(box, bytes);
    return Term(term_tag::BoxedProcBin::create_from_ptr<word_t>(box));
  } else {
    // Large bin, with boxed refcount and pointer
    vm::Heap *binheap = VM::get_heap(VM::HEAP_LARGE_BINARY);
    layout::HeapbinBox *box = vm::Heap::alloc<layout::HeapbinBox>(
                              binheap, layout::HEAP_BIN::box_size(bytes));
    box->set_byte_size(bytes);
    box->set_refcount(1);
    return Term(term_tag::BoxedHeapBin::create_from_ptr(box));
  }
}

void mfarity_t::println() {
  print();
  puts("");
}

void mfarity_t::print()
{
  mod.print();
  Std::fmt(":");
  fun.print();
  Std::fmt("/" FMT_UWORD, arity);
}

word_t layout::PROC_BIN::box_size(word_t bytes) {
  return calculate_word_size(bytes) + BOX_EXTRA;
}

word_t layout::HEAP_BIN::box_size(word_t bytes) {
  return calculate_word_size(bytes) + FAR_HEAP_EXTRA;
}

#endif // DEBUG

} // ns gluon
