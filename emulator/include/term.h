#pragma once

#include "defs.h"
#include "struct/str.h"

namespace gluon {

class VM;
namespace erts {
class Heap;
}  // g_heap.h
namespace proc {
class Heap;
}  // in g_heap.h
class Export;  // in g_module.h

namespace term_tag {

template <typename T>
static Word compress_pointer(T* p) {
  return reinterpret_cast<Word>(p);
}
template <typename T>
static T* expand_pointer(Word p) {
  return reinterpret_cast<T*>(p);
}

// Primary and immediate tagging:
//
// xxxx00 cons
// xxxx01 tuple
// xxxx10 boxed (subtag)
// 000011 atom
// 100011 catch index
// 001011 short pid
// 010011 short oid
// 011011 (unused)
// 101011 (unused) overloaded as reg X
// 110011 (unused) overloaded as slot Y
// 111011 nil,noval,rip
// xxx111 small integer

enum class PrimaryTag {
  Cons = 0,
  Tuple = 1,
  Boxed = 2,
  Immediate1 = 3,
};

//
// Group of operations on level 0 tag (stores tag in bits 0,1)
//
const Word Primary_tag_size = 2;

template <PrimaryTag TagType>
struct Level0Tag {
  const static Word Mask = 0x3;
  constexpr static bool check(Word x) { return (x & Mask) == (Word)TagType; }
  template <typename T>
  static Word create_from_ptr(T* p) {
    return compress_pointer(p) | (Word)TagType;
  }
  template <typename T>
  static T* expand_ptr(Word x) {
    return expand_pointer<T>(x & ~Mask);
  }
  constexpr static Word create(Word v) {
    return (v << Primary_tag_size) | (Word)TagType;
  }
  constexpr static Word value(Word t) { return (t >> Primary_tag_size); }
};

typedef Level0Tag<PrimaryTag::Cons> Cons;
typedef Level0Tag<PrimaryTag::Tuple> Tuple;
typedef Level0Tag<PrimaryTag::Boxed> Boxed;
typedef Level0Tag<PrimaryTag::Immediate1> Immed;

//
// Templatized tag/untag/check functions for level 1 tags (bits 2,3 when
// bits 0,1 are equal to IMMED1)
//
enum class Imm1Tag {
  Atom = 0,  //(0 << PRIMARY_SIZE) | IMMED1,
  SmallInt = 1,
  ShortPid = 2,
  ShortPort = 4,
  FpRegister = 6,
  Catch = 8,
  XRegister = 10,
  YRegister = 12,
  Special = 14,  // includes nil,noval,rip
};

//
// Group of operations on level 1 tag (stores tag in bits 2,3,4,5 while bits
// 0,1 contain 0x03)
//
template <Imm1Tag TagType>
struct Level1Tag {
  const static Word Mask = 0x3F;  // 6 least bits = 1
  // level 1 tag bits shifted left | level 0 Immediate1 tag
  const static Word Tag_incl_L0L1 =
      ((Word)TagType << Primary_tag_size) | (Word)PrimaryTag::Immediate1;
  const static Word L1_tag_bits = 6;

  constexpr static bool check(Word x) { return (x & Mask) == Tag_incl_L0L1; }
  constexpr static Word create(Word v) {
    return (v << L1_tag_bits) | Tag_incl_L0L1;
  }
  constexpr static Word value(Word t) { return t >> L1_tag_bits; }
};

typedef Level1Tag<Imm1Tag::Atom> Atom;
typedef Level1Tag<Imm1Tag::ShortPid> ShortPid;
typedef Level1Tag<Imm1Tag::ShortPort> ShortPort;
typedef Level1Tag<Imm1Tag::Catch> Catch;
typedef Level1Tag<Imm1Tag::FpRegister> FpRegister;
typedef Level1Tag<Imm1Tag::XRegister> XRegister;
typedef Level1Tag<Imm1Tag::YRegister> YRegister;
typedef Level1Tag<Imm1Tag::Special> Special;  // includes nil,noval,rip

struct Smallint {
  const static Word Tag = 0x1;
  const static Word Mask = 0x7;

  // level 1 bits shifted left with IMMED1 level 0 tag together
  const static Word Tag_incl_L0L1 =
      (Tag << Primary_tag_size) | (Word)PrimaryTag::Immediate1;
  const static Word L1_tag_bits = 3;

  constexpr static bool check(Word x) { return (x & Mask) == Tag_incl_L0L1; }
  constexpr static Word create(SWord v) {
    return (Word)(v << L1_tag_bits) | Tag_incl_L0L1;
  }
  constexpr static Word create_u(Word v) {
    return (Word)(v << L1_tag_bits) | Tag_incl_L0L1;
  }
  constexpr static SWord value(Word t) { return ((SWord)t) >> L1_tag_bits; }
  constexpr static Word value_u(Word t) { return t >> L1_tag_bits; }
};

//
// Boxed subtags
//
const Word boxed_subtag_bits = 4;
const Word boxed_subtag_mask = 0x0F;
const Word boxed_max_subtag_val =
    (1UL << (gluon::word_bitsize - boxed_subtag_bits)) - 1;

inline Word boxed_subtag(Word* p) {
  return p[0] & boxed_subtag_mask;
}

enum class BoxedSubtag {
  PositiveBignum = 0,
  NegativeBignum = 1,
  Float = 2,
  Map = 3,
  FunObject = 6,
  Export = 7,
  Pid = 8,
  Port = 9,
  Ref = 10,
  DestroyedSomething = 11,
  ProcBinary = 12,
  HeapBinary = 13,
  MatchContext = 14,
  SubBinary = 15,
};

// Takes least 4 bits of subtag
static constexpr BoxedSubtag get_subtag(Word x) {
  return (BoxedSubtag)(x & boxed_subtag_mask);
}
// Removes least 4 bits of subtag returning what's left
static constexpr Word get_subtag_value(Word x) {
  return x >> boxed_subtag_bits;
}
// Takes m_val from Term, converts to pointer and reads subtag (least 4 bits)
static BoxedSubtag unbox_and_get_subtag(Word x) {
  return (BoxedSubtag)(Boxed::expand_ptr<Word>(x)[0] & boxed_subtag_mask);
}

template <BoxedSubtag Subtag>
struct TaggedBox {
  // Takes a term value, converts to pointer, checks if it was boxed, then
  // follows the pointer and checks that first word is tagged with SUBTAG
  static bool unbox_and_check(Word x) {
    return Boxed::check(x) && unbox_and_get_subtag(x) == Subtag;
  }
  static constexpr bool check_subtag(Word x) { return get_subtag(x) == Subtag; }
  template <typename T>
  static Word create_from_ptr(T* p) {
    return Boxed::create_from_ptr<T>(p);
  }
  template <typename T>
  static T* expand_ptr(Word x) {
    return Boxed::expand_ptr<T>(x);
  }
  static constexpr Word create_subtag(Word x) {
    return (x << boxed_subtag_bits) | (Word)Subtag;
  }
};

typedef TaggedBox<BoxedSubtag::PositiveBignum> BoxedPosBignum;
typedef TaggedBox<BoxedSubtag::NegativeBignum> BoxedNegBignum;
typedef TaggedBox<BoxedSubtag::Float> BoxedFloat;
typedef TaggedBox<BoxedSubtag::Map> BoxedMap;
typedef TaggedBox<BoxedSubtag::FunObject> BoxedFun;
typedef TaggedBox<BoxedSubtag::Export> BoxedExport;
typedef TaggedBox<BoxedSubtag::Pid> BoxedPid;
typedef TaggedBox<BoxedSubtag::Port> BoxedPort;
typedef TaggedBox<BoxedSubtag::Ref> BoxedRef;
typedef TaggedBox<BoxedSubtag::DestroyedSomething> BoxedRIP;
typedef TaggedBox<BoxedSubtag::ProcBinary> BoxedProcBin;
typedef TaggedBox<BoxedSubtag::HeapBinary> BoxedHeapBin;
typedef TaggedBox<BoxedSubtag::MatchContext> BoxedMatchCtx;
typedef TaggedBox<BoxedSubtag::SubBinary> BoxedSubBin;

const Word continuation_tag = 1UL << (gluon::word_bitsize - 1);
// Check highest bit if it was CP pushed on stack
constexpr bool is_cp_word(Word x) {
  return continuation_tag == (x & continuation_tag);
}
template <typename T>
constexpr bool is_cp(T* x) {
  return is_cp_word((Word)x);
}

// Set highest bit to mark CP pushed on stack
template <typename T>
T* make_cp(T* x) {
  G_ASSERT(!is_cp(x));
  return (T*)(((Word)x) | continuation_tag);
}
// Check and clear highest bit to mark CP pushed on stack
template <typename T>
T* untag_cp(T* x) {
  G_ASSERT(is_cp(x));
  return (T*)((Word)x & (~continuation_tag));
}
}  // ns term_tag

namespace temporary {

#if FEATURE_FLOAT
const Word DOUBLE_DATA_WORDS = sizeof(Float) / sizeof(Word);
const Word FLOAT_SIZE_OBJECT = DOUBLE_DATA_WORDS + 1;
#endif

const Word PORT_DATA_SIZE = 28;
const Word PORT_NUM_SIZE = PORT_DATA_SIZE;
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

//
// Define layouts of boxes for boxed terms, tuple and cons
//
namespace layout {
// Cons layout has no place for bit tag
struct Cons {
  static const Word box_word_size = 2;

  template <typename Cell>
  static Cell& head(Cell* box) {
    static_assert(sizeof(Cell) == sizeof(Word), "bad cell size");
    return box[0];
  }

  template <typename Cell>
  static Cell& tail(Cell* box) {
    static_assert(sizeof(Cell) == sizeof(Word), "bad cell size");
    return box[1];
  }

  template <typename Cell>
  static Cell& element(Cell* box, Word i) {
    static_assert(sizeof(Cell) == sizeof(Word), "bad cell size");
    return box[i];
  }
};

// Tuple layout has no place for bit tag.
// First goes arity, then elements
struct Tuple {
  static const Word box_extra_words = 1;

  static Word box_size(Word Arity) { return Arity + box_extra_words; }

  template <typename Cell>
  static Cell& arity(Cell* box) {
    static_assert(sizeof(Cell) == sizeof(Word), "bad cell size");
    return box[0];
  }

  template <typename Cell>
  static Cell& element(Cell* box, Word i) {
    static_assert(sizeof(Cell) == sizeof(Word), "bad cell size");
    return box[i + box_extra_words];
  }
};

// Process-heap (small) and heap (large) binary layout
struct Binary {
  // both types of binary have size in first (subtag) word
  static Word get_byte_size(Word* p) {
    return term_tag::get_subtag_value(p[0]);
  }
};

// Box structure
// Word { size, tag_bits: 4 }; u8_t data[size]
struct ProcBin {
  static const Word box_extra_words = 1;

  static Word box_size(Word bytes);

  static void set_byte_size(Word* box, Word bytes) {
    box[0] = term_tag::BoxedProcBin::create_subtag(bytes);
  }

  template <typename Cell>
  static Cell* data(Cell* box) {
    static_assert(sizeof(Cell) == sizeof(Word), "bad cell size");
    return box + 1;
  }
};

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wzero-length-array"
// Heapbin is stored on far heap with refcount field
// Box only contains size and pointer to far heap with heapbin
class HeapbinBox {
 private:
  Word m_size;  // contains both size and boxed tag
  Word m_refcount;
  Word m_data[0];

 public:
  void set_byte_size(Word bytes) {
    m_size = term_tag::BoxedHeapBin::create_subtag(bytes);
  }

  template <typename Cell>
  Cell* data() {
    // static_assert(sizeof(Cell) == sizeof(Word), "bad cell size");
    return (Cell*)&m_data;
  }

  Word refcount() const { return m_refcount; }
  void set_refcount(Word r) { m_refcount = r; }
};
#pragma clang diagnostic pop

struct HeapBin {
  static const Word farheap_extra_words = sizeof(HeapbinBox) / sizeof(Word);

  static Word box_size(Word bytes);
};
}  // ns layout

// This wrap is here to make strong type difference between hardware hw::Word
// (which is just a machine size unsigned integer) and term type, which is
// complex bitfield structure.
// Note: Wrapping an integer with class is efficient if all members are inline
// and optimizations are on.
class Term {
 private:
  Word value_;

 public:
  explicit constexpr Term(Word v) : value_(v) {}
  constexpr Term() : value_(term::non_value_as_word) {}
  // constexpr Term(const Term &other): m_val(other.m_val) {}

  // Do not call this; for better looking code use gluon::NIL const instead
  constexpr static Term make_nil_() { return Term(term::nil_as_word); }
  // Do not call this; for better looking code use gluon::NONVALUE const instead
  constexpr static Term make_non_value_() {
    return Term(term::non_value_as_word);
  }

  //
  // Bit/arithmetic/comparisons
  // const
  void set(Word x) { value_ = x; }
  void set(const Term& t) { value_ = t.value_; }

  // Term operator <<(Word bits) const { return Term(m_val << bits); }
  constexpr Word as_word() const { return value_; }
  bool operator<(const Term& x) const { return value_ < x.value_; }
  bool operator==(const Term& x) const { return value_ == x.value_; }
  bool operator==(const Word x) const { return value_ == x; }
  bool operator!=(const Term& x) const { return value_ != x.value_; }
  bool operator!=(const Word x) const { return value_ != x; }
  bool is_nil() const { return value_ == term::nil_as_word; }
  bool is_not_nil() const { return value_ != term::nil_as_word; }
  bool is_non_value() const { return value_ == term::non_value_as_word; }
  bool is_value() const { return value_ != term::non_value_as_word; }

  bool is_immed() const { return term_tag::Immed::check(value_); }
  static bool are_both_immed(Term a, Term b) {
    return term_tag::Immed::check(a.as_word() & b.as_word());
  }

  //
  // Boxed pointer magic
  //
  template <typename T>
  T* boxed_get_ptr() const {
    G_ASSERT(is_boxed() || is_tuple() || is_cons());
    return term_tag::Boxed::expand_ptr<T>(value_);
  }
  // Same as boxed_get_ptr but is usable on masked CP boxes which may point to
  // random stuff
  template <typename T>
  T* boxed_get_ptr_unchecked() const {
    return term_tag::Boxed::expand_ptr<T>(value_);
  }
  template <typename T>
  static Term make_boxed(T* x) {
    return Term(term_tag::Boxed::create_from_ptr<T>(x));
  }
  template <typename T>
  constexpr static Term make_boxed_cp(T* x) {
    return Term(term_tag::Boxed::create_from_ptr<T>(term_tag::make_cp(x)));
  }
  bool is_boxed() const { return term_tag::Boxed::check(value_); }
  Word boxed_get_subtag() const {
    G_ASSERT(is_boxed());
    Word* p = boxed_get_ptr<Word>();
    if (!p) {
      return (Word)-1;
    }
    return term_tag::boxed_subtag(p);
  }

  //
  // Throw/catch exception magic
  //
  constexpr bool is_catch() const { return term_tag::Catch::check(value_); }
  constexpr static Term make_catch(Word x) {
    return Term(term_tag::Catch::create(x));
  }
  constexpr Word catch_val() const { return term_tag::Catch::value(value_); }

  //
  // Cons
  // Cons is a boxed value pointing at two words in memory: a head and a tail
  //
  // TODO: make a cool variadic helper to build lists
  static Term allocate_cons(proc::Heap* heap, Term head, Term tail);
  static Term make_cons(Term* box) {
    return Term(term_tag::Cons::create_from_ptr<Term>(box));
  }
  bool is_cons() const { return term_tag::Cons::check(value_); }
  bool is_list() const { return is_nil() || is_cons(); }
  Term cons_head() const { return cons_get_element(0); }
  Term cons_tail() const { return cons_get_element(1); }
  // Takes head and tail at once
  void cons_head_tail(Term& h, Term& t) const {
    auto p = boxed_get_ptr<Word>();
    h = Term(layout::Cons::head(p));
    t = Term(layout::Cons::tail(p));
  }
  bool is_cons_printable() const;
  // Unfolds list into linear array using limit as array max size
  Word cons_to_array(Term* arr, Word limit);

 protected:
  static bool is_cons_printable_element(Term el);
  static bool does_char_require_quoting(Word c) {
    return c == '\\' || c == '\"';
  }
  Term cons_get_element(Word n) const {
    G_ASSERT(n == 0 || n == 1);
    auto p = boxed_get_ptr<Word>();
    return Term(layout::Cons::element(p, n));
  }

 public:
  //
  // Atoms
  //
  constexpr static Term make_atom(Word x) {
    return Term(term_tag::Atom::create(x));
  }
  constexpr bool is_atom() const { return term_tag::Atom::check(value_); }
  constexpr Word atom_val() const { return term_tag::Atom::value(value_); }
  Str atom_str(const VM& vm) const;
  const char* atom_c_str(const VM& vm) const { return atom_str(vm).c_str(); }

  //
  // Small Integer
  //
  static constexpr Term make_small(SWord x) {
    return Term(term_tag::Smallint::create(x));
  }
  static constexpr Term make_small_u(Word x) {
    return Term(term_tag::Smallint::create_u(x));
  }
  static constexpr bool does_fit_into_small(SWord n) {
    return term::small_lower_bound <= n && n <= term::small_upper_bound;
  }
  constexpr bool is_small() const { return term_tag::Smallint::check(value_); }
  constexpr bool is_integer() const {
// Match either small int or big int if feature is enabled
#if FEATURE_BIGNUM
    return is_small() || is_big();
#else
    return is_small();
#endif
  }
  SWord small_sword() const {
    G_ASSERT(is_small());
    //    Std::fmt("small_get_s val=" FMT_0xHEX " val=" FMT_0xHEX "\n", m_val,
    //    term_tag::Smallint::value(m_val));
    return term_tag::Smallint::value(value_);
  }
  Word small_word() const {
    G_ASSERT(is_small());
    Word v = term_tag::Smallint::value_u(value_);
    return (Word)v;
  }
  static bool are_both_small(Term a, Term b) {
    return term_tag::Smallint::check(a.as_word() & b.as_word());
  }

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

  //
  // Pid
  //
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
  static Term make_short_pid(Word data) {
    return Term(term_tag::ShortPid::create(data));
  }
  constexpr bool is_short_pid() const {
    return term_tag::ShortPid::check(value_);
  }
  constexpr bool is_pid() const {
#if FEATURE_ERL_DIST
    return is_short_pid() || term_tag::BoxedPid::check(m_val);
#else
    return is_short_pid();
#endif
  }
  constexpr Word short_pid_get_value() const {
    return term_tag::ShortPid::value(value_);
  }
  //
  // Port id (Outled id, Oid)
  //
  constexpr bool is_short_port() const {
    return term_tag::ShortPort::check(value_);
  }
  constexpr bool is_port() const {
    return is_short_port() || term_tag::BoxedPort::unbox_and_check(value_);
  }
  constexpr Word short_port_get_value() const {
    return term_tag::ShortPort::value(value_);
  }

  //
  // Tuple
  //
  static Term make_zero_tuple() {
    return Term(term_tag::Tuple::create_from_ptr(&term::g_zero_sized_tuple));
  }
  // NOTE: Elements should contain 1 extra slot for arity!
  static Term make_tuple(Term* elements, Word arity) {
    layout::Tuple::arity((Word*)elements) = arity;
    return Term(term_tag::Tuple::create_from_ptr(elements));
  }
  // Does not set arity field, assuming that element values are already all set
  static Term make_tuple_prepared(Term* elements) {
    return Term(term_tag::Tuple::create_from_ptr(elements));
  }
  constexpr bool is_tuple() const { return term_tag::Tuple::check(value_); }
  Word tuple_get_arity() const {
    G_ASSERT(is_tuple());
    auto p = boxed_get_ptr<Word>();
    return layout::Tuple::arity(p);
  }
  // Zero based index n
  Term tuple_get_element(Word n) const {
    auto p = boxed_get_ptr<Word>();
    G_ASSERT(layout::Tuple::arity(p) > n);
    return Term(layout::Tuple::element(p, n));
  }
  // Zero based index n
  void tuple_set_element(Word n, Term t) const {
    auto p = boxed_get_ptr<Word>();
    G_ASSERT(layout::Tuple::arity(p) > n);
    layout::Tuple::element(p, n) = t.as_word();
  }

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
#endif

  //
  // Special values
  //
  constexpr bool is_regx() const { return term_tag::XRegister::check(value_); }
  static constexpr Term make_regx(Word x) {
    return Term(term_tag::XRegister::create(x));
  }
  constexpr Word regx_get_value() const {
    return term_tag::XRegister::value(value_);
  }
  constexpr bool is_regfp() const {
    if (feature_float) {
      return term_tag::FpRegister::check(value_);
    }
    return false;
  }
  static constexpr Term make_regfp(Word x) {
    if (feature_float) {
      return Term(term_tag::FpRegister::create(x));
    }
    return make_non_value_();
  }
  constexpr Word regfp_get_value() const {
    if (feature_float) {
      return term_tag::FpRegister::value(value_);
    }
    return 0;
  }

  constexpr bool is_regy() const { return term_tag::YRegister::check(value_); }
  static constexpr Term make_regy(Word x) {
    return Term(term_tag::YRegister::create(x));
  }
  constexpr Word regy_get_value() const {
    return term_tag::YRegister::value(value_);
  }

  bool is_boxed_fun() const {
    return term_tag::BoxedFun::unbox_and_check(value_);
  }

  bool is_boxed_export() const {
    return term_tag::BoxedExport::unbox_and_check(value_);
  }
  static Term make_boxed_export(Export* ex) {
    // Assuming that pointer has subtag in first word of memory already
    Word val = term_tag::BoxedExport::create_from_ptr(ex);
    G_ASSERT(term_tag::BoxedExport::unbox_and_check(val));
    return Term(val);
  }

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
  static Term make_binary(VM& vm, proc::Heap* h, Word bytes);

  bool is_proc_binary() const {
    return term_tag::BoxedProcBin::unbox_and_check(value_);
  }
  bool is_heap_binary() const {
    return term_tag::BoxedHeapBin::unbox_and_check(value_);
  }
  bool is_binary() const {
    // return is_heap_binary() || is_proc_binary();
    Word* p = boxed_get_ptr<Word>();
    return term_tag::BoxedProcBin::check_subtag(p[0]) ||
           term_tag::BoxedHeapBin::check_subtag(p[0]);
  }
  Word binary_get_size() const {
    G_ASSERT(is_binary());  // this is slow but debug only
    Word* p = boxed_get_ptr<Word>();
    // both types of binary have size in first (subtag) word
    return layout::Binary::get_byte_size(p);
  }
  template <typename T>
  T* binary_get() const {
    G_ASSERT(is_binary());  // this is slow but debug only
    Word* p = boxed_get_ptr<Word>();
    if (term_tag::BoxedProcBin::check_subtag(p[0])) {
      return (T*)layout::ProcBin::data(p);
    }
    G_ASSERT(term_tag::BoxedHeapBin::check_subtag(p[0]));
    // Get pointer to large binary, add 1 word offset and cast it to T *
    layout::HeapbinBox* far_box = (layout::HeapbinBox*)p;
    return far_box->data<T>();
  }

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
