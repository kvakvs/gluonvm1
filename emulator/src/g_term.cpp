#include "g_term.h"
#include "g_vm.h"
#include "g_heap.h"
#include "g_code_server.h"
#include "g_fun.h"
#include "g_module.h" // for export_t class
#include "g_term_helpers.h"
#include "g_heap.h"
#include "g_binary.h"

#if G_TEST
#include <fructose/fructose.h>
#endif

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
    printf("NOT_A_TERM");
    return;
  }
  if (is_cons()) {
    if (is_cons_printable()) {
      // list is printable - print quotes and every character except tail
      printf("\"");
      word_t c = (u8_t)cons_head().small_get_unsigned();
      if (does_char_require_quoting(c)) {
        printf("\\");
      }
      printf("%c", (u8_t)c);
      Term item = cons_tail();
      while (item.is_cons()) {
        c = item.cons_head().small_get_unsigned();
        if (does_char_require_quoting(c)) {
          printf("\\");
        }
        printf("%c", (u8_t)c);
        item = item.cons_tail();
      }
      printf("\"");
    } else {
      // not printable - dump terms and tail
      printf("[");
      cons_head().print();
      Term item = cons_tail();
      while (item.is_cons()) {
        printf(",");
        item.cons_head().print();
        item = item.cons_tail();
      }
      if (!item.is_nil()) {
        printf("|");
        item.print();
      }
      printf("]");
    }
  }
  else if (is_tuple()) {
    auto arity = tuple_get_arity();
    printf("{");
    for (word_t n = 0; n < arity; ++n) {
      tuple_get_element(n).print();
      if (n < arity-1) {
        printf(",");
      }
    }
    printf("}");
  }
  else if (is_boxed()) {
    auto p = boxed_get_ptr<word_t>();
    if (term_tag::is_cp<word_t>(p)) {
      word_t *cp = term_tag::untag_cp<word_t>(p);
      printf("#CP<");
      VM::get_cs()->print_mfa(cp);
      printf(">");
      return;
    }
    if (is_boxed_fun()) {
      printf("#Fun<");
      auto bf = boxed_get_ptr<boxed_fun_t>();
//      if ((word_t)(bf->fun_entry) < 0x1000) {
//        printf(FMT_0xHEX, (word_t)bf->fun_entry);
//      } else {
        VM::get_cs()->print_mfa(bf->fun_entry->code);
//      }
      printf(">");
      return;
    }
    if (is_boxed_export()) {
      printf("#ExportedFun<");
      auto ex = boxed_get_ptr<export_t>();
      ex->mfa.print();
      printf(";");
      if (ex->is_bif()) {
        printf("bif");
      } else {
        VM::get_cs()->print_mfa(ex->code);
      }
      printf(">");
      return;
    }    printf("#Box<Tag=" FMT_UWORD ";", boxed_get_subtag());
    VM::get_cs()->print_mfa(boxed_get_ptr<word_t>());
    printf(">");
  }
  else if (is_nil()) {
    printf("[]");
  }
  else if (is_non_value()) {
    printf("NON_VALUE");
  }
  else if (is_atom()) {
    printf("'%s'", atom_str().c_str());
  }
  else if (is_small()) {
    printf(FMT_SWORD, small_get_signed());
  }
  else if (is_catch()) {
    printf("CATCH(" FMT_0xHEX ")", catch_val());
  }
  else if (is_short_pid()) {
    printf("#Pid<" FMT_0xHEX ">", short_pid_get_value());
  }
  else if (is_regx()) {
    printf("X[" FMT_UWORD "]", regx_get_value());
  }
#if FEATURE_FLOAT
  else if (is_regfp()) {
    printf("FP[" FMT_UWORD "]", regfp_get_value());
  }
#endif
  else if (is_regy()) {
    printf("Y[" FMT_UWORD "]", regy_get_value());
  }
  else {
    printf("UNKNOWN(" FMT_0xHEX ")", m_val);
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
    word_t *box = vm::Heap::alloc<word_t>(binheap,
                                          layout::HEAP_BIN::box_size(bytes));
    layout::HEAP_BIN::set_byte_size(box, bytes);
    layout::HEAP_BIN::refcount(box) = 1;
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
  printf(":");
  fun.print();
  printf("/" FMT_UWORD, arity);
}

word_t layout::PROC_BIN::box_size(word_t bytes) {
  return calculate_word_size(bytes) + BOX_EXTRA;
}

word_t layout::HEAP_BIN::box_size(word_t bytes) {
  return calculate_word_size(bytes) + BOX_EXTRA;
}

#endif // DEBUG

//
//====================================
//
#if G_TEST
} // ns gluon

#include "bif/g_bif_misc.h"
#pragma clang diagnostic ignored "-Wweak-vtables"
namespace gluon {
struct term_test_t: public fructose::test_base<term_test_t>
{
  void test_term_basics(const std::string& test_name) {
    Term t_tuple_el[10];
    Term t_tuple = Term::make_tuple(t_tuple_el, 10);
    fructose_assert(t_tuple.is_tuple());
  }
  void test_term_cmp(const std::string &tn) {
    Term l1 = Term::allocate_cons(nullptr, Term::make_small(3), NIL);
    Term l2 = Term::allocate_cons(nullptr, Term::make_small(2), l1);
    Term l3 = Term::allocate_cons(nullptr, Term::make_small(1), l2);

    Term m1 = Term::allocate_cons(nullptr, Term::make_small(1), NIL);
    Term m2 = Term::allocate_cons(nullptr, Term::make_small(2), m1);
    Term m3 = Term::allocate_cons(nullptr, Term::make_small(3), m2);

    fructose_assert(bif::are_terms_equal(l3, m3, false) == false);
  }
  void test_term_small(const std::string& test_name) {
    Term s1 = Term::make_small(-1);
    Term s2 = Term::make_small(0);
    Term s3 = Term::make_small(1);
    fructose_assert(s1.small_get_signed() == -1);
    fructose_assert(s2.small_get_signed() == 0);
    fructose_assert(s3.small_get_signed() == 1);
  }
}; // struct

void term_test(int argc, const char *argv[]) {
  term_test_t tests;
  tests.add_test("term_basics", &term_test_t::test_term_basics);
  tests.add_test("term_cmp", &term_test_t::test_term_cmp);
  tests.add_test("term_small", &term_test_t::test_term_small);
  tests.run(argc, const_cast<char **>(argv));
}

#endif // TEST


} // ns gluon
