#pragma once

#include "g_defs.h"

namespace gluon {

namespace vm { class Heap; } // g_heap.h
namespace proc { class Heap; } // in g_heap.h
class export_t; // in g_module.h

namespace term_tag {

//  const word_t IMMED1_SIZE   = 6;
//  const word_t IMMED1_MASK   = 0x3F;

  template <typename T>
  inline static word_t compress_pointer(T *p) {
    return reinterpret_cast<word_t>(p);
  }
  template <typename T>
  inline static T *expand_pointer(word_t p) {
    return reinterpret_cast<T *>(p);
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

  enum {
    CONS   = 0,
    TUPLE  = 1,
    BOXED  = 2,
    IMMED1 = 3,
  };

  //
  // Group of operations on level 0 tag (stores tag in bits 0,1)
  //
  const word_t PRIMARY_SIZE = 2;

  template <word_t TAG> struct LEVEL0_TAG {
    const static word_t MASK = 0x3;
    constexpr static bool check(word_t x) {
      return (x & MASK) == TAG;
    }
    template <typename T>
    inline static word_t create_from_ptr(T *p) {
      return compress_pointer(p) | TAG;
    }
    template <typename T>
    inline static T *expand_ptr(word_t x) {
      return expand_pointer<T>(x & ~MASK);
    }
    constexpr static word_t create(word_t v) {
      return (v << PRIMARY_SIZE) | TAG;
    }
    constexpr static word_t value(word_t t) {
      return (t >> PRIMARY_SIZE);
    }
  };

  typedef LEVEL0_TAG<CONS> Cons;
  typedef LEVEL0_TAG<TUPLE> Tuple;
  typedef LEVEL0_TAG<BOXED> Boxed;
  typedef LEVEL0_TAG<IMMED1> Immed;

  //
  // Templatized tag/untag/check functions for level 1 tags (bits 2,3 when
  // bits 0,1 are equal to IMMED1)
  //
  enum {
    ATOM      = 0, //(0 << PRIMARY_SIZE) | IMMED1,
    SMALL_INT = 1,
    SHORT_PID = 2,
    SHORT_PORT = 4,
    FP_REG    = 6,
    CATCH     = 8,
    X_REG     = 10,
    Y_REG     = 12,
    SPECIAL   = 14, // includes nil,noval,rip
  };

  //
  // Group of operations on level 1 tag (stores tag in bits 2,3,4,5 while bits
  // 0,1 contain 0x03)
  //
  template <word_t TAG> struct LEVEL1_TAG {
    const static word_t MASK = 0x3F;
    // level 1 bits shifted left with IMMED1 level 0 tag together
    const static word_t TAG_L0_L1 = (TAG << PRIMARY_SIZE) | IMMED1;
    const static word_t L1_TAG_BITS = 6;

    constexpr static bool check(word_t x) {
      return (x & MASK) == TAG_L0_L1;
    }
    constexpr static word_t create(word_t v) {
      return (v << L1_TAG_BITS) | TAG_L0_L1;
    }
    constexpr static word_t value(word_t t) {
      return t >> L1_TAG_BITS;
    }
  };

  typedef LEVEL1_TAG<ATOM> Atom;
  typedef LEVEL1_TAG<SHORT_PID> ShortPid;
  typedef LEVEL1_TAG<SHORT_PORT> ShortPort;
  typedef LEVEL1_TAG<CATCH> Catch;
  typedef LEVEL1_TAG<FP_REG> FloatRegReference;
  typedef LEVEL1_TAG<X_REG> RegReference;
  typedef LEVEL1_TAG<Y_REG> StackReference;
  typedef LEVEL1_TAG<SPECIAL> Special; // includes nil,noval,rip

  struct Smallint {
    const static word_t TAG           = 0x1;
    const static word_t MASK          = 0x7;

    // level 1 bits shifted left with IMMED1 level 0 tag together
    const static word_t TAG_L0_L1 = (TAG << PRIMARY_SIZE) | IMMED1;
    const static word_t L1_TAG_BITS = 3;

    constexpr static bool check(word_t x) {
      return (x & MASK) == TAG_L0_L1;
    }
    constexpr static word_t create(sword_t v) {
      return (word_t)(v << L1_TAG_BITS) | TAG_L0_L1;
    }
    constexpr static word_t create_u(word_t v) {
      return (word_t)(v << L1_TAG_BITS) | TAG_L0_L1;
    }
    constexpr static sword_t value(word_t t) {
      return ((sword_t)t) >> L1_TAG_BITS;
    }
    constexpr static word_t value_u(word_t t) {
      return t >> L1_TAG_BITS;
    }
  };

  //
  // Boxed subtags
  //
  const word_t BOXED_SUBTAG_BITS = 4;
  const word_t BOXED_SUBTAG_MASK = 0x0F;
  const word_t BOXED_MAX_SUBTAG_VALUE
    = (1UL << (get_hardware_bits() - BOXED_SUBTAG_BITS)) - 1;

  inline word_t boxed_subtag(word_t *p) {
    return p[0] & BOXED_SUBTAG_MASK;
  }

  enum {
    BOXED_POS_BIGNUM  = 0,
    BOXED_NEG_BIGNUM  = 1,
#if FEATURE_FLOAT
    BOXED_FLOAT       = 2,
#endif
    BOXED_MAP         = 3,
    BOXED_FUN         = 6,
    BOXED_EXPORT      = 7,
    BOXED_PID         = 8,
    BOXED_PORT        = 9,
    BOXED_REF         = 10,
    BOXED_RIP         = 11,
    BOXED_PROC_BIN    = 12,
    BOXED_HEAP_BIN    = 13,
    BOXED_MATCH_CTX   = 14,
    BOXED_SUB_BIN     = 15,
  };

  // Takes least 4 bits of subtag
  static constexpr word_t get_subtag(word_t x) {
    return x & BOXED_SUBTAG_MASK;
  }
  // Removes least 4 bits of subtag returning what's left
  static constexpr word_t get_subtag_value(word_t x) {
    return x >> BOXED_SUBTAG_BITS;
  }
  // Takes m_val from Term, converts to pointer and reads subtag (least 4 bits)
  static inline word_t unbox_and_get_subtag(word_t x) {
    return Boxed::expand_ptr<word_t>(x)[0] & BOXED_SUBTAG_MASK;
  }

  template <word_t SUBTAG> struct BOXED_SUBTAG {
    // Takes a term value, converts to pointer, checks if it was boxed, then
    // follows the pointer and checks that first word is tagged with SUBTAG
    static inline bool unbox_and_check(word_t x) {
      return Boxed::check(x) && unbox_and_get_subtag(x) == SUBTAG;
    }
    static constexpr bool check_subtag(word_t x) {
      return get_subtag(x) == SUBTAG;
    }
    template <typename T>
    inline static word_t create_from_ptr(T *p) {
      return Boxed::create_from_ptr<T>(p);
    }
    template <typename T>
    inline static T *expand_ptr(word_t x) {
      return Boxed::expand_ptr<T>(x);
    }
    static constexpr word_t create_subtag(word_t x) {
      return (x << BOXED_SUBTAG_BITS) | SUBTAG;
    }
  };

  typedef BOXED_SUBTAG<BOXED_POS_BIGNUM> BoxedPosBignum;
  typedef BOXED_SUBTAG<BOXED_NEG_BIGNUM> BoxedNegBignum;
#if FEATURE_FLOAT
  typedef BOXED_SUBTAG<BOXED_FLOAT> BoxedFloat;
#endif
  typedef BOXED_SUBTAG<BOXED_MAP> BoxedMap;
  typedef BOXED_SUBTAG<BOXED_FUN> BoxedFun;
  typedef BOXED_SUBTAG<BOXED_EXPORT> BoxedExport;
  typedef BOXED_SUBTAG<BOXED_PID> BoxedPid;
  typedef BOXED_SUBTAG<BOXED_PORT> BoxedPort;
  typedef BOXED_SUBTAG<BOXED_REF> BoxedRef;
  typedef BOXED_SUBTAG<BOXED_RIP> BoxedRIP;
  typedef BOXED_SUBTAG<BOXED_PROC_BIN> BoxedProcBin;
  typedef BOXED_SUBTAG<BOXED_HEAP_BIN> BoxedHeapBin;
  typedef BOXED_SUBTAG<BOXED_MATCH_CTX> BoxedMatchCtx;
  typedef BOXED_SUBTAG<BOXED_SUB_BIN> BoxedSubBin;

  const word_t CP_TAG = 1UL << (G_HARDWARE_BITS-1);
  // Check highest bit if it was CP pushed on stack
  template <typename T>
  inline constexpr bool is_cp(T *x) {
    return 0 != (((word_t)x) & CP_TAG);
  }
  // Set highest bit to mark CP pushed on stack
  template <typename T>
  inline T *make_cp(T *x) {
    G_ASSERT(!is_cp(x));
    return (T *)(((word_t)x) | CP_TAG);
  }
  // Check and clear highest bit to mark CP pushed on stack
  template <typename T>
  inline T *untag_cp(T *x) {
    G_ASSERT(is_cp(x));
    return (T *)(((word_t)x) & (~CP_TAG));
  }
} // ns term_tag

namespace temporary {

#if FEATURE_FLOAT
  const word_t DOUBLE_DATA_WORDS = sizeof(float_t)/sizeof(word_t);
  const word_t FLOAT_SIZE_OBJECT = DOUBLE_DATA_WORDS+1;
#endif

  const word_t PORT_DATA_SIZE   = 28;
  const word_t PORT_NUM_SIZE    = PORT_DATA_SIZE;
} // ns temporary

namespace dist {
  //
  // Creation in node specific data (pids, ports, refs)
  //
  const word_t CRE_SIZE = 2;

  typedef u8_t creation_t;

  // MAX value for the creation field in pid, port and reference
  const creation_t MAX_CREATION  = (1 << CRE_SIZE);
  const creation_t ORIG_CREATION     = 0;
  const creation_t INTERNAL_CREATION = 255;
} // ns dist

namespace term {
  const word_t NIL = term_tag::Special::create(~0UL);
  const word_t THE_NON_VALUE = term_tag::Special::create(0);

  const word_t PID_ID_SIZE = 15;
  const word_t PID_DATA_SIZE = 28;
  const word_t PID_SER_SIZE = (PID_DATA_SIZE - PID_ID_SIZE);

  const word_t SMALL_BITS = sizeof(word_t) * 8
                            - term_tag::Smallint::L1_TAG_BITS;
  const sword_t UPPER_BOUND = (1L << (SMALL_BITS-1))-1;
  const sword_t LOWER_BOUND = -(1L << (SMALL_BITS-1));

  extern word_t g_zero_sized_tuple;
  extern word_t g_zero_sized_map;
} // ns term

class boxed_fun_t;

//
// Define layouts of boxes for boxed terms, tuple and cons
//
namespace layout {
  // Cons layout has no place for bit tag
  struct CONS {
    static const word_t BOX_SIZE = 2;

    template <typename Cell>
    static inline Cell &head(Cell *box) {
      static_assert(sizeof(Cell) == sizeof(word_t), "bad cell size");
      return box[0];
    }

    template <typename Cell>
    static inline Cell &tail(Cell *box) {
      static_assert(sizeof(Cell) == sizeof(word_t), "bad cell size");
      return box[1];
    }

    template <typename Cell>
    static inline Cell &element(Cell *box, word_t i) {
      static_assert(sizeof(Cell) == sizeof(word_t), "bad cell size");
      return box[i];
    }
  };

  // Tuple layout has no place for bit tag.
  // First goes arity, then elements
  struct TUPLE {
    static const word_t BOX_EXTRA = 1;

    static inline word_t box_size(word_t Arity) { return Arity + BOX_EXTRA; }

    template <typename Cell>
    static inline Cell &arity(Cell *box) {
      static_assert(sizeof(Cell) == sizeof(word_t), "bad cell size");
      return box[0];
    }

    template <typename Cell>
    static inline Cell &element(Cell *box, word_t i) {
      static_assert(sizeof(Cell) == sizeof(word_t), "bad cell size");
      return box[i+BOX_EXTRA];
    }
  };

  // Process-heap (small) and heap (large) binary layout
  struct BINARY {
    // both types of binary have size in first (subtag) word
    static inline word_t get_byte_size(word_t *p) {
      return term_tag::get_subtag_value(p[0]);
    }
  };

  // Box structure
  // word_t { size, tag_bits: 4 }; u8_t data[size]
  struct PROC_BIN {
    static const word_t BOX_EXTRA = 1;

    static inline word_t box_size(word_t bytes);

    static inline void set_byte_size(word_t *box, word_t bytes) {
      box[0] = term_tag::BoxedProcBin::create_subtag(bytes);
    }

    template <typename Cell>
    static inline Cell *data(Cell *box) {
      static_assert(sizeof(Cell) == sizeof(word_t), "bad cell size");
      return box+1;
    }
  };

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wzero-length-array"
    // Heapbin is stored on far heap with refcount field
    // Box only contains size and pointer to far heap with heapbin
    class HeapbinBox {
    private:
      word_t m_size; // contains both size and boxed tag
      word_t m_refcount;
      word_t m_data[0];

    public:
      inline void set_byte_size(word_t bytes) {
        m_size = term_tag::BoxedHeapBin::create_subtag(bytes);
      }

      template <typename Cell>
      inline Cell *data() {
        //static_assert(sizeof(Cell) == sizeof(word_t), "bad cell size");
        return (Cell *)&m_data;
      }

      word_t refcount() const {
        return m_refcount;
      }
      void set_refcount(word_t r) {
        m_refcount = r;
      }
    };
#pragma clang diagnostic pop

  struct HEAP_BIN {
    static const word_t FAR_HEAP_EXTRA = sizeof(HeapbinBox) / sizeof(word_t);

    static inline word_t box_size(word_t bytes);
  };
} // ns layout

// This wrap is here to make strong type difference between hardware hw::Word
// (which is just a machine size unsigned integer) and term type, which is
// complex bitfield structure.
// Note: Wrapping an integer with class is efficient if all members are inline
// and optimizations are on.
class Term {
private:
  word_t m_val;

public:
  explicit constexpr Term(word_t v): m_val(v) {}
  constexpr Term(): m_val(0) {}
  //constexpr Term(const Term &other): m_val(other.m_val) {}

  // Do not call this; for better looking code use gluon::NIL const instead
  constexpr static Term make_nil_() {
    return Term(term::NIL);
  }
  // Do not call this; for better looking code use gluon::NONVALUE const instead
  constexpr static Term make_non_value_() {
    return Term(term::THE_NON_VALUE);
  }

  //
  // Bit/arithmetic/comparisons
  //const
  inline void set(word_t x) { m_val = x; }
  inline void set(const Term &t) { m_val = t.m_val; }

  //inline Term operator <<(word_t bits) const { return Term(m_val << bits); }
  inline constexpr word_t as_word() const { return m_val; }
  inline bool operator <(const Term &x) const { return m_val < x.m_val; }
  inline bool operator ==(const Term &x) const { return m_val == x.m_val; }
  inline bool operator ==(const word_t x) const { return m_val == x; }
  inline bool operator !=(const Term &x) const { return m_val != x.m_val; }
  inline bool operator !=(const word_t x) const { return m_val != x; }
  inline bool is_nil() const { return m_val == term::NIL; }
  inline bool is_not_nil() const { return m_val != term::NIL; }
  inline bool is_non_value() const { return m_val == term::THE_NON_VALUE; }
  inline bool is_value() const { return m_val != term::THE_NON_VALUE; }

  inline bool is_immed() const {
    return term_tag::Immed::check(m_val);
  }
  inline static bool are_both_immed(Term a, Term b) {
    return term_tag::Immed::check(a.as_word() & b.as_word());
  }

  //
  // Boxed pointer magic
  //
  template <typename T> inline T *boxed_get_ptr() const {
    G_ASSERT(is_boxed() || is_tuple() || is_cons());
    return term_tag::Boxed::expand_ptr<T>(m_val);
  }
  template <typename T> inline static Term make_boxed(T *x) {
    return Term(term_tag::Boxed::create_from_ptr<T>(x));
  }
  template <typename T> constexpr static Term make_boxed_cp(T *x) {
    return Term(term_tag::Boxed::create_from_ptr<T>(term_tag::make_cp(x)));
  }
  inline bool is_boxed() const {
    return term_tag::Boxed::check(m_val);
  }
  inline word_t boxed_get_subtag() const {
    G_ASSERT(is_boxed());
    word_t *p = boxed_get_ptr<word_t>();
    if (!p) { return (word_t)-1; }
    return term_tag::boxed_subtag(p);
  }

  //
  // Throw/catch exception magic
  //
  inline constexpr bool is_catch() const {
    return term_tag::Catch::check(m_val);
  }
  constexpr static Term make_catch(word_t x) {
    return Term(term_tag::Catch::create(x));
  }
  constexpr word_t catch_val() const {
    return term_tag::Catch::value(m_val);
  }

  //
  // Cons
  // Cons is a boxed value pointing at two words in memory: a head and a tail
  //
  // TODO: make a cool variadic helper to build lists
  static Term allocate_cons(proc::Heap *heap, Term head, Term tail);
  inline static Term make_cons(Term *box) {
    return Term(term_tag::Cons::create_from_ptr<Term>(box));
  }
  inline bool is_cons() const {
    return term_tag::Cons::check(m_val);
  }
  inline bool is_list() const {
    return is_nil() || is_cons();
  }
  inline Term cons_head() const { return cons_get_element(0); }
  inline Term cons_tail() const { return cons_get_element(1); }
  // Takes head and tail at once
  inline void cons_head_tail(Term &h, Term &t) const {
    auto p = boxed_get_ptr<word_t>();
    h = Term(layout::CONS::head(p));
    t = Term(layout::CONS::tail(p));
  }
  bool is_cons_printable() const;
  // Unfolds list into linear array using limit as array max size
  word_t cons_to_array(Term *arr, word_t limit);
protected:
  static bool is_cons_printable_element(Term el);
  static bool does_char_require_quoting(word_t c) {
    return c == '\\' || c == '\"';
  }
  inline Term cons_get_element(word_t n) const {
    G_ASSERT(n == 0 || n == 1);
    auto p = boxed_get_ptr<word_t>();
    return Term(layout::CONS::element(p, n));
  }

public:
  //
  // Atoms
  //
  constexpr static Term make_atom(word_t x) {
    return Term(term_tag::Atom::create(x));
  }
  constexpr bool is_atom() const {
    return term_tag::Atom::check(m_val);
  }
  constexpr word_t atom_val() const {
    return term_tag::Atom::value(m_val);
  }
  Str atom_str() const;
  const char *atom_c_str() const { return atom_str().c_str(); }

  //
  // Small Integer
  //
  static constexpr Term make_small(sword_t x) {
    return Term(term_tag::Smallint::create(x));
  }
  static constexpr Term make_small_u(word_t x) {
    return Term(term_tag::Smallint::create_u(x));
  }
  static constexpr bool does_fit_into_small(sword_t n) {
    return term::LOWER_BOUND <= n && n <= term::UPPER_BOUND;
  }
  constexpr bool is_small() const {
    return term_tag::Smallint::check(m_val);
  }
  constexpr bool is_integer() const {
    // Match either small int or big int if feature is enabled
#if FEATURE_BIGNUM
    return is_small() || is_big();
#else
    return is_small();
#endif
  }
  inline sword_t small_get_signed() const {
    G_ASSERT(is_small());
//    Std::fmt("small_get_s val=" FMT_0xHEX " val=" FMT_0xHEX "\n", m_val, term_tag::Smallint::value(m_val));
    return term_tag::Smallint::value(m_val);
  }
  inline word_t small_get_unsigned() const {
    G_ASSERT(is_small());
    word_t v = term_tag::Smallint::value_u(m_val);
    return (word_t)v;
  }
  inline static bool are_both_small(Term a, Term b) {
    return term_tag::Smallint::check(a.as_word() & b.as_word());
  }

#if FEATURE_BIGNUM
  //
  // Big integer (bignum)
  //
  inline bool is_big() const {
    return is_boxed() && boxed_val()->is_bignum_header();
  }
  inline Term *boxed_val() const {
    return expand_pointer(m_val - term_tag::PRIMARY_BOXED);
  }
  inline bool is_bignum_header() const {
    return (m_val & (term_tag::HEADER_MASK - term_tag::BIG_SIGN_BIT))
            == term_tag::HEADER_POS_BIG;
  }
  inline word_t bignum_header_arity() const {
    return (m_val >> term_tag::HEADER_ARITY_OFFS);
  }
  inline word_t big_arity() const {
    return boxed_val()->bignum_header_arity();
  }
  inline bignum::digit_t big_v() const {
    return *(bignum::digit_t *)(boxed_val()+1);
  }
#endif

  //
  // Pid
  //
  static constexpr bool is_valid_pid_id(word_t x) {
    return x < (1 << term::PID_ID_SIZE) - 1;
  }
  static constexpr bool is_valid_pid_serial(word_t x) {
    return x < (1 << term::PID_SER_SIZE) - 1;
  }
  static constexpr word_t make_pid_data(word_t ser, word_t num) {
      return (word_t)(ser << term::PID_ID_SIZE | num);
  }
  // Data arg is created using Term::make_pid_data
  static Term make_short_pid(word_t data) {
    return Term(term_tag::ShortPid::create(data));
  }
  constexpr bool is_short_pid() const {
    return term_tag::ShortPid::check(m_val);
  }
  constexpr bool is_pid() const {
#if FEATURE_ERL_DIST
    return is_short_pid() || term_tag::BoxedPid::check(m_val);
#else
    return is_short_pid();
#endif
  }
  constexpr word_t short_pid_get_value() const {
    return term_tag::ShortPid::value(m_val);
  }
  //
  // Port id (Outled id, Oid)
  //
  constexpr bool is_short_port() const {
    return term_tag::ShortPort::check(m_val);
  }
  constexpr bool is_port() const {
    return is_short_port() || term_tag::BoxedPort::unbox_and_check(m_val);
  }
  constexpr word_t short_port_get_value() const {
    return term_tag::ShortPort::value(m_val);
  }

  //
  // Tuple
  //
  static inline Term make_zero_tuple() {
    return Term(term_tag::Tuple::create_from_ptr(&term::g_zero_sized_tuple));
  }
  // NOTE: Elements should contain 1 extra slot for arity!
  static inline Term make_tuple(Term *elements, word_t arity) {
    layout::TUPLE::arity((word_t *)elements) = arity;
    return Term(term_tag::Tuple::create_from_ptr(elements));
  }
  // Does not set arity field, assuming that element values are already all set
  static inline Term make_tuple_prepared(Term *elements) {
    return Term(term_tag::Tuple::create_from_ptr(elements));
  }
  constexpr bool is_tuple() const {
    return term_tag::Tuple::check(m_val);
  }
  inline word_t tuple_get_arity() const {
    G_ASSERT(is_tuple());
    auto p = boxed_get_ptr<word_t>();
    return layout::TUPLE::arity(p);
  }
  // Zero based index n
  inline Term tuple_get_element(word_t n) const {
    auto p = boxed_get_ptr<word_t>();
    G_ASSERT(layout::TUPLE::arity(p) > n);
    return Term(layout::TUPLE::element(p, n));
  }
  // Zero based index n
  inline void tuple_set_element(word_t n, Term t) const {
    auto p = boxed_get_ptr<word_t>();
    G_ASSERT(layout::TUPLE::arity(p) > n);
    layout::TUPLE::element(p, n) = t.as_word();
  }

#if FEATURE_MAPS
  //
  // Map
  //
  static inline Term make_zero_map() {
    return Term(term_tag::Boxed::create_from_ptr(&term::g_zero_sized_map));
  }
  // NOTE: Elements should contain 1 extra slot for arity!
  static inline Term make_map(Term *kv_pairs, word_t arity) {
    kv_pairs[0] = arity;
    return Term(term_tag::Tuple::create_from_ptr(kv_pairs+1));
  }
#endif

#if G_DEBUG
  void print() const;
  void println() const;
#endif

  //
  // Special values
  //
  constexpr bool is_regx() const {
    return term_tag::RegReference::check(m_val);
  }
  static constexpr Term make_regx(word_t x) {
    return Term(term_tag::RegReference::create(x));
  }
  constexpr word_t regx_get_value() const {
    return term_tag::RegReference::value(m_val);
  }
#if FEATURE_FLOAT
  constexpr bool is_regfp() const {
    return term_tag::FloatRegReference::check(m_val);
  }
  static constexpr Term make_regfp(word_t x) {
    return Term(term_tag::FloatRegReference::create(x));
  }
  constexpr word_t regfp_get_value() const {
    return term_tag::FloatRegReference::value(m_val);
  }
#endif
  constexpr bool is_regy() const {
    return term_tag::StackReference::check(m_val);
  }
  static constexpr Term make_regy(word_t x) {
    return Term(term_tag::StackReference::create(x));
  }
  constexpr word_t regy_get_value() const {
    return term_tag::StackReference::value(m_val);
  }

  inline bool is_boxed_fun() const {
    return term_tag::BoxedFun::unbox_and_check(m_val);
  }

  inline bool is_boxed_export() const {
    return term_tag::BoxedExport::unbox_and_check(m_val);
  }
  static inline Term make_boxed_export(export_t *ex) {
    // Assuming that pointer has subtag in first word of memory already
    word_t val = term_tag::BoxedExport::create_from_ptr(ex);
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
  static Term make_binary(proc::Heap *h, word_t bytes);

  inline bool is_proc_binary() const {
    return term_tag::BoxedProcBin::unbox_and_check(m_val);
  }
  inline bool is_heap_binary() const {
    return term_tag::BoxedHeapBin::unbox_and_check(m_val);
  }
  inline bool is_binary() const {
    //return is_heap_binary() || is_proc_binary();
    word_t *p = boxed_get_ptr<word_t>();
    return term_tag::BoxedProcBin::check_subtag(p[0])
        || term_tag::BoxedHeapBin::check_subtag(p[0]);
  }
  inline word_t binary_get_size() const {
    G_ASSERT(is_binary()); // this is slow but debug only
    word_t *p = boxed_get_ptr<word_t>();
    // both types of binary have size in first (subtag) word
    return layout::BINARY::get_byte_size(p);
  }
  template <typename T>
  inline T *binary_get() const {
    G_ASSERT(is_binary()); // this is slow but debug only
    word_t *p = boxed_get_ptr<word_t>();
    if (term_tag::BoxedProcBin::check_subtag(p[0])) {
      return (T *)layout::PROC_BIN::data(p);
    }
    G_ASSERT(term_tag::BoxedHeapBin::check_subtag(p[0]));
    // Get pointer to large binary, add 1 word offset and cast it to T *
    layout::HeapbinBox *far_box = (layout::HeapbinBox *)p;
    return far_box->data<T>();
  }
};

#define G_IS_BOOLEAN(T) ((T) == atom::TRUE || (T) == atom::FALSE)

const static Term NONVALUE = Term::make_non_value_();
const static Term NIL = Term::make_nil_();

static_assert(sizeof(Term) == sizeof(word_t),
              "Term size should be same as machine word");

// A pair of atom and int arity, can be used as map key
//typedef Pair<Term, word_t> fun_arity_t;
class fun_arity_t {
public:
  Term fun = NONVALUE;
  word_t arity = 0;

  fun_arity_t() {}
  fun_arity_t(Term f, word_t a): fun(f), arity(a) {}
  inline bool operator <(const fun_arity_t &x) const {
    return fun < x.fun || (fun == x.fun && arity < x.arity);
  }
};

class mfarity_t {
public:
  Term    mod;
  Term    fun;
  word_t  arity;
  mfarity_t(): mod(NONVALUE), fun(NONVALUE), arity(0) {}
  mfarity_t(Term m, Term f, word_t a): mod(m), fun(f), arity(a) {
    G_ASSERT(a <= vm::MAX_FUN_ARITY);
  }
  mfarity_t(Term m, const fun_arity_t &fa)
    : mod(m), fun(fa.fun), arity(fa.arity)
  {
    G_ASSERT(arity <= vm::MAX_FUN_ARITY);
  }
  fun_arity_t as_funarity() const {
    return fun_arity_t(fun, arity);
  }
#if G_DEBUG
  void print();
  void println();
#endif
};

class Process;
typedef Term (*bif0_fn)(Process *);
typedef Term (*bif1_fn)(Process *, Term);
typedef Term (*bif2_fn)(Process *, Term, Term);
typedef Term (*bif3_fn)(Process *, Term, Term, Term);

#if G_TEST
void term_test(int argc, const char *argv[]);
#endif // TEST

constexpr Term operator "" _usmall(unsigned long long int x) {
    return Term::make_small_u(x);
}
constexpr Term operator "" _small(unsigned long long int x) {
    return Term::make_small((signed long long int)x);
}

} // ns gluon
