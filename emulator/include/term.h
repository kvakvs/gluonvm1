#pragma once

#include "defs.h"
#include "pointer.h"
#include "struct/str.h"
#include "term_tag.h"

namespace gluon {

class VM;
namespace erts {
class Heap;
}  // in heap.h
namespace proc {
class Heap;
}  // in heap.h
class Export;  // in module.h

namespace temporary {

const Word float_data_words = sizeof(Float) / sizeof(Word);
const Word float_size_object = float_data_words + 1;  // TODO: layout namespace

const Word port_data_size = 28;
const Word port_num_size = port_data_size;
}  // ns temporary

namespace dist {
//
// Creation in node specific data (pids, ports, refs)
//
const Word creation_size = 2;

using Creation = Uint8;

// MAX value for the creation field in pid, port and reference
const Creation max_creation = (1 << creation_size);
const Creation orig_creation = 0;
const Creation internal_creation = 255;
}  // ns dist

namespace term {
const Word nil_as_word = term_tag::Special::create(~0UL);
const Word non_value_as_word = term_tag::Special::create(0);

const Word pid_id_size = 15;
const Word pid_data_size = 28;
const Word pid_serial_size = (pid_data_size - pid_id_size);

const Word small_bits = sizeof(Word) * 8 - term_tag::Smallint::L1_tag_bits;
const SWord small_upper_bound = (1L << (small_bits - 1)) - 1;
const SWord small_lower_bound = -(1L << (small_bits - 1));

extern Word g_zero_sized_tuple;
extern Word g_zero_sized_map;
}  // ns term

class BoxedFun;

#include "term_layout.h"

namespace term {

// This wrap is here to make strong type difference between hardware hw::Word
// (which is just a machine size unsigned integer) and term type, which is
// complex bitfield structure.
// Note: Wrapping an integer with class is efficient if all members are inline
// and optimizations are on.
class TermStorage {
 private:
  Word value_;

 public:
  explicit constexpr TermStorage(Word v) : value_(v) {}
  constexpr TermStorage() : value_(term::non_value_as_word) {}

  void set_value(Word v) { value_ = v; }
  Word value() const { return value_; }
};

//
// Boxed pointer magic
// This is compiled as part of Term class and adds boxed functions to it
//
template <typename TERM>
class BoxedAspect {
 private:
  constexpr TERM* self() { return static_cast<TERM*>(this); }
  constexpr const TERM* self() const { return static_cast<const TERM*>(this); }

 public:
  template <typename T>
  T* boxed_get_ptr() const {
    G_ASSERT(self()->is_boxed() || self()->is_tuple() || self()->is_cons());
    return term_tag::Boxed::expand_ptr<T>(self()->value());
  }
  CodePointer boxed_get_codeptr() const {
    return CodePointer(term_tag::Boxed::expand_ptr<Word>(self()->value()));
  }

  // Same as boxed_get_ptr but is usable on masked CP boxes which may point to
  // random stuff
  template <typename T>
  T* boxed_get_ptr_unchecked() const {
    return term_tag::Boxed::expand_ptr<T>(self()->value());
  }
  template <typename T>
  static TERM make_boxed(T* x) {
    return TERM(term_tag::Boxed::create_from_ptr<T>(x));
  }

  bool is_boxed() const { return term_tag::Boxed::check(self()->value()); }

  Word boxed_get_subtag() const {
    G_ASSERT(is_boxed());
    Word* p = boxed_get_ptr<Word>();
    if (!p) {
      return (Word)-1;
    }
    return term_tag::boxed_subtag(p);
  }
};

//
// Cons
// Cons is a boxed value pointing at two words in memory: a head and a tail
// This is compiled as part of Term class and adds CONS functions to it
//
template <typename TERM>
class ConsAspect {
 private:
  constexpr TERM* self() { return static_cast<TERM*>(this); }
  constexpr const TERM* self() const { return static_cast<const TERM*>(this); }

 public:
  // TODO: make a cool variadic helper to build lists
  static TERM allocate_cons(proc::Heap* heap, TERM head, TERM tail);
  static TERM make_cons(TERM* box) {
    return TERM(term_tag::Cons::create_from_ptr<TERM>(box));
  }
  bool is_cons() const { return term_tag::Cons::check(self()->value()); }
  bool is_list() const { return self()->is_nil() || self()->is_cons(); }
  TERM cons_head() const { return cons_get_element(0); }
  TERM cons_tail() const { return cons_get_element(1); }
  // Takes head and tail at once
  void cons_head_tail(TERM& h, TERM& t) const {
    auto p = self()->template boxed_get_ptr<Word>();
    h = TERM(layout::Cons::head(p));
    t = TERM(layout::Cons::tail(p));
  }
  bool is_cons_printable() const;
  // Unfolds list into linear array using limit as array max size
  Word cons_to_array(TERM* arr, Word limit) const {
    if (self()->is_nil()) {
      return 0;
    }

    *arr = cons_head();
    arr++;

    TERM i = cons_tail();
    Word n = 1;

    while (n < limit && i.is_cons()) {
      // splits i into head and tail, tail is assigned to i again
      i.cons_head_tail(*arr, i);
      ++arr;
      ++n;
    }
    return n;
  }
  Str cons_to_str() const {
    Str result;

    if (self()->is_nil()) {
      return result;
    }

    TERM i = *self();
    while (i.is_cons()) {
      // splits i into head and tail, tail is assigned to i again
      TERM hd;
      i.cons_head_tail(hd, i);
      // FIXME: Unicode
      result += (char)hd.small_word();
    }
    return result;
  }

 protected:
  static bool is_cons_printable_element(TERM el) {
    if (!el.is_small()) {
      return false;
    }
    Word c = el.small_word();
    return (c >= ' ' && c <= 127);
  }
  static bool does_char_require_quoting(Word c) {
    return c == '\\' || c == '\"';
  }
  TERM cons_get_element(Word n) const {
    G_ASSERT(n == 0 || n == 1);
    auto p = self()->template boxed_get_ptr<Word>();
    return TERM(layout::Cons::element(p, n));
  }
};

//
// Atoms
// This is compiled as part of Term class and adds Atom functions to it
//
template <typename TERM>
class AtomAspect {
 private:
  constexpr TERM* self() { return static_cast<TERM*>(this); }
  constexpr const TERM* self() const { return static_cast<const TERM*>(this); }

 public:
  constexpr static TERM make_atom(Word x) {
    return TERM(term_tag::Atom::create(x));
  }
  constexpr bool is_atom() const {
    return term_tag::Atom::check(self()->value());
  }
  constexpr Word atom_val() const {
    return term_tag::Atom::value(self()->value());
  }
  Str atom_str(const VM& vm) const;
  const char* atom_c_str(const VM& vm) const { return atom_str(vm).c_str(); }
};

//
// Small integer
// This is compiled as part of Term class and adds small* functions to it
//
template <typename TERM>
class SmallintAspect {
 private:
  constexpr TERM* self() { return static_cast<TERM*>(this); }
  constexpr const TERM* self() const { return static_cast<const TERM*>(this); }

 public:
  //
  // Small Integer
  //
  static constexpr TERM make_small(SWord x) {
    return TERM(term_tag::Smallint::create(x));
  }
  static constexpr TERM make_small_u(Word x) {
    return TERM(term_tag::Smallint::create_u(x));
  }
  static constexpr bool does_fit_into_small(SWord n) {
    return term::small_lower_bound <= n && n <= term::small_upper_bound;
  }
  constexpr bool is_small() const {
    return term_tag::Smallint::check(self()->value());
  }
  constexpr bool is_integer() const {
// Match either small int or big int if feature is enabled
#if FEATURE_BIGNUM
    return self()->is_small() || self()->is_big();
#else
    return self()->is_small();
#endif
  }
  SWord small_sword() const {
    G_ASSERT(is_small());
    //    Std::fmt("small_get_s val=" FMT_0xHEX " val=" FMT_0xHEX "\n", m_val,
    //    term_tag::Smallint::value(m_val));
    return term_tag::Smallint::value(self()->value());
  }
  Word small_word() const {
    G_ASSERT(is_small());
    Word v = term_tag::Smallint::value_u(self()->value());
    return (Word)v;
  }
  static bool are_both_small(TERM a, TERM b) {
    return term_tag::Smallint::check(a.value() & b.value());
  }
};

//
// Pid handling (short local Pids)
// This is compiled as part of Term class and adds pid functions to it
//
template <typename TERM>
class PidAspect {
 private:
  constexpr TERM* self() { return static_cast<TERM*>(this); }
  constexpr const TERM* self() const { return static_cast<const TERM*>(this); }

 public:
  static constexpr bool is_valid_pid_id(Word x) {
    return x < (1 << term::pid_id_size) - 1;
  }
  static constexpr bool is_valid_pid_serial(Word x) {
    return x < (1 << term::pid_serial_size) - 1;
  }
  static constexpr Word make_pid_data(Word ser, Word num) {
    return (Word)(ser << term::pid_id_size | num);
  }
  // Data arg is created using Term::make_pid_data
  static TERM make_short_pid(Word data) {
    return TERM(term_tag::ShortPid::create(data));
  }
  constexpr bool is_short_pid() const {
    return term_tag::ShortPid::check(self()->value());
  }
  bool is_remote_pid() const {
    return term_tag::BoxedPid::unbox_and_check(self()->value());
  }
  bool is_pid() const { return is_short_pid() || is_remote_pid(); }
  constexpr Word short_pid_get_value() const {
    return term_tag::ShortPid::value(self()->value());
  }
};

//
// Port handling
// This is compiled as part of Term class and adds port functions to it
//
template <typename TERM>
class PortAspect {
 private:
  constexpr TERM* self() { return static_cast<TERM*>(this); }
  constexpr const TERM* self() const { return static_cast<const TERM*>(this); }

 public:
  constexpr bool is_short_port() const {
    return term_tag::ShortPort::check(self()->value());
  }
  bool is_remote_port() const {
    return term_tag::BoxedPort::unbox_and_check(self()->value());
  }
  bool is_port() const { return is_short_port() || is_remote_port(); }
  constexpr Word short_port_get_value() const {
    return term_tag::ShortPort::value(self()->value());
  }
};

//
// Tuple support
// This is compiled as part of Term class and adds tuple functions to it
//
template <typename TERM>
class TupleAspect {
 private:
  constexpr TERM* self() { return static_cast<TERM*>(this); }
  constexpr const TERM* self() const { return static_cast<const TERM*>(this); }

 public:
  static TERM make_zero_tuple() {
    return TERM(term_tag::Tuple::create_from_ptr(&term::g_zero_sized_tuple));
  }
  // NOTE: Elements should contain 1 extra slot for arity!
  static TERM make_tuple(TERM* elements, Word arity) {
    layout::Tuple::arity((Word*)elements) = arity;
    return TERM(term_tag::Tuple::create_from_ptr(elements));
  }
  // Does not set arity field, assuming that element values are already all set
  static TERM make_tuple_prepared(TERM* elements) {
    return TERM(term_tag::Tuple::create_from_ptr(elements));
  }
  constexpr bool is_tuple() const {
    return term_tag::Tuple::check(self()->value());
  }
  Word tuple_get_arity() const {
    G_ASSERT(is_tuple());
    auto p = self()->template boxed_get_ptr<Word>();
    return layout::Tuple::arity(p);
  }
  // Zero based index n
  TERM tuple_get_element(Word n) const {
    auto p = self()->template boxed_get_ptr<Word>();
    G_ASSERT(layout::Tuple::arity(p) > n);
    return TERM(layout::Tuple::element(p, n));
  }
  // Zero based index n
  void tuple_set_element(Word n, TERM t) const {
    auto p = self()->template boxed_get_ptr<Word>();
    G_ASSERT(layout::Tuple::arity(p) > n);
    layout::Tuple::element(p, n) = t.value();
  }
};

//
// Special value aspect
// This is compiled as part of Term class and adds extra functions to it
//
template <typename TERM>
class SpecialValueAspect {
 private:
  constexpr TERM* self() { return static_cast<TERM*>(this); }
  constexpr const TERM* self() const { return static_cast<const TERM*>(this); }

 public:
  constexpr bool is_regx() const {
    return term_tag::XRegister::check(self()->value());
  }
  static constexpr TERM make_regx(Word x) {
    return TERM(term_tag::XRegister::create(x));
  }
  constexpr Word regx_get_value() const {
    return term_tag::XRegister::value(self()->value());
  }
  constexpr bool is_regfp() const {
    if (feature_float) {
      return term_tag::FpRegister::check(self()->value());
    }
    return false;
  }
  static constexpr TERM make_regfp(Word x) {
    if (feature_float) {
      return TERM(term_tag::FpRegister::create(x));
    }
    return TERM::make_non_value_();
  }
  constexpr Word regfp_get_value() const {
    if (feature_float) {
      return term_tag::FpRegister::value(self()->value());
    }
    return 0;
  }

  constexpr bool is_regy() const {
    return term_tag::YRegister::check(self()->value());
  }
  static constexpr TERM make_regy(Word x) {
    return TERM(term_tag::YRegister::create(x));
  }
  constexpr Word regy_get_value() const {
    return term_tag::YRegister::value(self()->value());
  }

  bool is_boxed_fun() const {
    return term_tag::BoxedFun::unbox_and_check(self()->value());
  }

  bool is_boxed_export() const {
    return term_tag::BoxedExport::unbox_and_check(self()->value());
  }
  static TERM make_boxed_export(Export* ex) {
    // Assuming that pointer has subtag in first word of memory already
    Word val = term_tag::BoxedExport::create_from_ptr(ex);
    G_ASSERT(term_tag::BoxedExport::unbox_and_check(val));
    return TERM(val);
  }
};

//
// Binaries small and large
//
// Small size binaries are allocated on the local heap (BOXED_PROC_BIN subtag)
// There is no refcount, always copy
// --boxedptr--> <<Size, Subtag:4>> Bytes[size]
//
// Large binaries are refcounted on the large binary heap (BOXED_HEAP_BIN)
// Large binary refers to a size=2 box where p[1] is pointer to large heap bin
// First word of large heap bin is refcount
// --boxedptr--> <<Size, Subtag:4>> --data-ptr--> Refcount Bytes[size]
//
// This is compiled as part of Term class and adds binary* functions to it
//
template <typename TERM>
class BinaryAspect {
 private:
  constexpr TERM* self() { return static_cast<TERM*>(this); }
  constexpr const TERM* self() const { return static_cast<const TERM*>(this); }

 public:
  static TERM make_binary(VM& vm, proc::Heap* h, Word bytes);

  bool is_proc_binary() const {
    return term_tag::BoxedProcBin::unbox_and_check(self()->value());
  }
  bool is_heap_binary() const {
    return term_tag::BoxedHeapBin::unbox_and_check(self()->value());
  }
  bool is_binary() const {
    // return is_heap_binary() || is_proc_binary();
    Word* p = self()->template boxed_get_ptr<Word>();
    return term_tag::BoxedProcBin::check_subtag(p[0]) ||
           term_tag::BoxedHeapBin::check_subtag(p[0]);
  }
  Word binary_get_size() const {
    G_ASSERT(is_binary());  // this is slow but debug only
    Word* p = self()->template boxed_get_ptr<Word>();
    // both types of binary have size in first (subtag) word
    return layout::Binary::get_byte_size(p);
  }
  template <typename T>
  T* binary_get() const {
    G_ASSERT(is_binary());  // this is slow but debug only
    Word* p = self()->template boxed_get_ptr<Word>();
    if (term_tag::BoxedProcBin::check_subtag(p[0])) {
      return (T*)layout::ProcBin::data(p);
    }
    G_ASSERT(term_tag::BoxedHeapBin::check_subtag(p[0]));
    // Get pointer to large binary, add 1 word offset and cast it to T *
    layout::HeapbinBox* far_box = (layout::HeapbinBox*)p;
    return far_box->data<T>();
  }
};

}  // ns term

//
// Do some CRTP inheritance here to split functionality into base classes
// because those chunks of functions rarely intersect
//
class Term : public term::TermStorage,
             public term::BoxedAspect<Term>,
             public term::ConsAspect<Term>,
             public term::AtomAspect<Term>,
             public term::SmallintAspect<Term>,
             public term::PidAspect<Term>,
             public term::PortAspect<Term>,
             public term::TupleAspect<Term>,
             public term::SpecialValueAspect<Term>,
             public term::BinaryAspect<Term> {
 public:
  explicit constexpr Term(Word v) : term::TermStorage(v) {}
  // explicit Term(ContinuationPointer cp)
  //  : term::TermStorage(cp.value()) {}
  constexpr Term() : term::TermStorage(term::non_value_as_word) {}

  // Do not call this; for better looking code use gluon::the_nil instead
  constexpr static Term make_nil_() { return Term(term::nil_as_word); }
  // Do not call this; for better looking code use gluon::the_non_value instead
  constexpr static Term make_non_value_() {
    return Term(term::non_value_as_word);
  }

  //
  // Bit/arithmetic/comparisons
  // const
  void set(const Term& t) { set_value(t.value()); }

  bool operator<(const Term& x) const { return value() < x.value(); }
  bool operator==(const Term& x) const { return value() == x.value(); }
  bool operator==(const Word x) const { return value() == x; }
  bool operator!=(const Term& x) const { return value() != x.value(); }
  bool operator!=(const Word x) const { return value() != x; }
  bool is_nil() const { return value() == term::nil_as_word; }
  bool is_not_nil() const { return value() != term::nil_as_word; }
  bool is_nonvalue() const { return value() == term::non_value_as_word; }
  bool is_not_nonvalue() const { return value() != term::non_value_as_word; }

  bool is_immed() const { return term_tag::Immed::check(value()); }
  static bool are_both_immed(Term a, Term b) {
    return term_tag::Immed::check(a.value() & b.value());
  }

  //
  // Throw/catch exception magic
  //
  constexpr bool is_catch() const { return term_tag::Catch::check(value()); }
  constexpr static Term make_catch(Word x) {
    return Term(term_tag::Catch::create(x));
  }
  constexpr Word catch_val() const { return term_tag::Catch::value(value()); }

#if FEATURE_BIGNUM
  //
  // Big integer (bignum)
  //
  bool is_big() const { return is_boxed() && boxed_val()->is_bignum_header(); }
  Term* boxed_val() const {
    return expand_pointer(m_val - term_tag::PRIMARY_BOXED);
  }
  bool is_bignum_header() const {
    return (m_val & (term_tag::HEADER_MASK - term_tag::BIG_SIGN_BIT)) ==
           term_tag::HEADER_POS_BIG;
  }
  Word bignum_header_arity() const {
    return (m_val >> term_tag::HEADER_ARITY_OFFS);
  }
  Word big_arity() const { return boxed_val()->bignum_header_arity(); }
  bignum::digit_t big_v() const { return *(bignum::digit_t*)(boxed_val() + 1); }
#endif

#if FEATURE_MAPS
  //
  // Map
  //
  static Term make_zero_map() {
    return Term(term_tag::Boxed::create_from_ptr(&term::g_zero_sized_map));
  }
  // NOTE: Elements should contain 1 extra slot for arity!
  static Term make_map(Term* kv_pairs, Word arity) {
    kv_pairs[0] = arity;
    return Term(term_tag::Tuple::create_from_ptr(kv_pairs + 1));
  }
#endif

#if G_DEBUG
  void print(const VM& vm) const;
  void println(const VM& vm) const;
#else
  void print(const VM& vm) const {}
  void println(const VM& vm) const {}
#endif

  bool is_reference() const {
    if (!is_boxed()) {
      return false;
    }
    Word* p = boxed_get_ptr<Word>();
    return term_tag::BoxedRef::check_subtag(p[0]);
  }
};

#define G_IS_BOOLEAN(T) ((T) == atom::TRUE || (T) == atom::FALSE)

const static Term the_non_value = Term::make_non_value_();
const static Term the_nil = Term::make_nil_();

static_assert(sizeof(Term) == sizeof(Word),
              "Term size should be same as machine word");

// A pair of atom and int arity, can be used as map key
// typedef Pair<Term, Word> fun_arity_t;
class FunArity {
 public:
  Term fun = the_non_value;
  Word arity = 0;

  FunArity() {}
  FunArity(Term f, Word a) : fun(f), arity(a) { G_ASSERT(f.is_atom()); }
  bool operator<(const FunArity& x) const {
    return fun < x.fun || (fun == x.fun && arity < x.arity);
  }
};

class MFArity {
 public:
  Term mod;
  Term fun;
  Word arity;
  MFArity() : mod(the_non_value), fun(the_non_value), arity(0) {}
  MFArity(Term m, Term f, Word a) : mod(m), fun(f), arity(a) {
    G_ASSERT(m.is_atom());
    G_ASSERT(f.is_atom());
    G_ASSERT(a <= erts::max_fun_arity);
  }
  MFArity(Term m, const FunArity& fa) : mod(m), fun(fa.fun), arity(fa.arity) {
    G_ASSERT(m.is_atom());
    G_ASSERT(arity <= erts::max_fun_arity);
  }

  MFArity(const MFArity&) = default;
  MFArity(MFArity&&) = delete;
  MFArity& operator=(const MFArity&) = default;
  MFArity& operator=(MFArity&&) = default;

  FunArity as_funarity() const { return FunArity(fun, arity); }
#if G_DEBUG
  void print(const VM& vm);
  void println(const VM& vm);
#endif
};

class Process;
typedef Term (*bif0_fn)(Process*);
typedef Term (*bif1_fn)(Process*, Term);
typedef Term (*bif2_fn)(Process*, Term, Term);
typedef Term (*bif3_fn)(Process*, Term, Term, Term);

template <Word NumArgs>
struct SelectBifFn {};
template <>
struct SelectBifFn<0> {
  using Type = bif0_fn;
  static Term apply(Type fn, Process* p, Term*) { return fn(p); }
};
template <>
struct SelectBifFn<1> {
  using Type = bif1_fn;
  static Term apply(Type fn, Process* p, Term* args) { return fn(p, args[0]); }
};
template <>
struct SelectBifFn<2> {
  using Type = bif2_fn;
  static Term apply(Type fn, Process* p, Term* args) {
    return fn(p, args[0], args[1]);
  }
};
template <>
struct SelectBifFn<3> {
  using Type = bif3_fn;
  static Term apply(Type fn, Process* p, Term* args) {
    return fn(p, args[0], args[1], args[2]);
  }
};

#if G_TEST
void term_test(int argc, const char* argv[]);
#endif  // TEST

// From now on you can define literal unsigned and signed small terms
// Do 1_usmall to define unsigned and 1_small to interpret it as signed
constexpr Term operator"" _usmall(unsigned long long int x) {
  return Term::make_small_u(x);
}
constexpr Term operator"" _small(unsigned long long int x) {
  return Term::make_small((signed long long int)x);
}

}  // ns gluon
