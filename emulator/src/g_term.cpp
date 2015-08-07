#include "g_term.h"
#include "g_vm.h"
#include "g_heap.h"
#include "g_code_server.h"
#include "g_fun.h"
#include "g_module.h" // for export_t class
#include "g_term_helpers.h"

#if G_TEST
#include <fructose/fructose.h>
#endif

namespace gluon {

word_t term::g_zero_sized_tuple = 0;

#if FEATURE_MAPS
word_t term::g_zero_sized_map = term_tag::BoxedMap::create_subtag(0);
#endif

Term Term::allocate_cons(proc::Heap *heap, Term head, Term tail) {
  Term *d = (Term *)heap->h_alloc(2);
  d[0] = head;
  d[1] = tail;
  return make_cons(d);
}

bool Term::is_cons_printable() const
{
  Term item = *this;
  while (item.is_cons()) {
    if (!is_cons_printable_element(item.cons_head())) {
      return false;
    }
    item = item.cons_tail();
  }
  return item.is_nil();
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
void Term::print()
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
      VM::get_cs()->print_mfa(bf->fe->code);
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
    }    printf("#Box<Tag=%zu;", boxed_get_subtag());
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
    printf("%zi", small_get_signed());
  }
  else if (is_catch()) {
    printf("CATCH(0x%zx)", catch_val());
  }
  else if (is_short_pid()) {
    printf("PID");
  }
  else if (is_regx()) {
    printf("X[%zu]", regx_get_value());
  }
#if FEATURE_FLOAT
  else if (is_regfp()) {
    printf("FP[%zu]", regfp_get_value());
  }
#endif
  else if (is_regy()) {
    printf("Y[%zu]", regy_get_value());
  }
  else {
    printf("UNKNOWN(%zx)", m_val);
  }
}

void Term::println()
{
  print();
  puts("");
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
  printf("/%zu", arity);
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
