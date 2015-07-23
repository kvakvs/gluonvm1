#pragma once

#include "g_defs.h"

namespace gluon {

class Heap;

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
    inline static T *value_ptr(word_t x) {
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
    constexpr static sword_t value(word_t t) {
      return (sword_t)t >> L1_TAG_BITS;
    }
  };

  //
  // Boxed subtags
  //
  const word_t BOXED_SUBTAG_BITS = 4;
  const word_t BOXED_SUBTAG_MASK = 0x0F;

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

  static inline word_t get_boxed_subtag(word_t x) {
    return Boxed::value_ptr<word_t>(x)[0] & BOXED_SUBTAG_MASK;
  }

  template <word_t SUBTAG> struct BOXED_SUBTAG {
    // Takes a term value, and checks if it is boxed and points at SUBTAG
    static inline bool check(word_t x) {
      return Boxed::check(x) && get_boxed_subtag(x) == SUBTAG;
    }
    template <typename T>
    inline static word_t create_from_ptr(T *p) {
      return Boxed::create_from_ptr<T>(p);
    }
    template <typename T>
    inline static T *value_ptr(word_t x) {
      return Boxed::value_ptr<T>(x);
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

  const word_t DOUBLE_DATA_WORDS = sizeof(float_t)/sizeof(word_t);
  const word_t FLOAT_SIZE_OBJECT = DOUBLE_DATA_WORDS+1;

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

  constexpr static Term make_nil() {
    return Term(term::NIL);
  }
  constexpr static Term make_non_value() {
    return Term(term::THE_NON_VALUE);
  }

  //
  // Bit/arithmetic/comparisons
  //const
  inline void set(word_t x) { m_val = x; }
  inline void set(const Term &t) { m_val = t.m_val; }

  //inline Term operator <<(word_t bits) const { return Term(m_val << bits); }
  inline constexpr word_t value() const { return m_val; }
  inline bool operator <(const Term &x) const { return m_val < x.m_val; }
  inline bool operator ==(const Term &x) const { return m_val == x.m_val; }
  inline bool operator ==(const word_t x) const { return m_val == x; }
  inline bool operator !=(const Term &x) const { return m_val != x.m_val; }
  inline bool operator !=(const word_t x) const { return m_val != x; }
  inline bool is_nil() const { return m_val == term::NIL; }
  inline bool is_non_value() const { return m_val == term::THE_NON_VALUE; }
  inline bool is_value() const { return m_val != term::THE_NON_VALUE; }

  inline bool is_immed() const {
    return term_tag::Immed::check(m_val);
  }
  inline static bool are_both_immed(Term a, Term b) {
    return term_tag::Immed::check(a.value() & b.value());
  }

  //
  // Pointer magic
  //
  template <typename T> inline T *boxed_get_ptr() const {
    return term_tag::Boxed::value_ptr<T>(m_val);
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
  static Term allocate_cons(Heap *heap, Term head, Term tail);
  inline static Term make_cons(Term *box) {
    return Term(term_tag::Cons::create_from_ptr<Term>(box));
  }
  inline bool is_cons() const {
    return term_tag::Cons::check(m_val);
  }
  inline Term cons_head() const { return cons_get_element(0); }
  inline Term cons_tail() const { return cons_get_element(1); }
protected:
  inline Term cons_get_element(word_t n) const {
    G_ASSERT(n == 0 || n == 1);
    auto p = boxed_get_ptr<word_t>();
    return Term(p[n]);
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

  //
  // Small Integer
  //
  static constexpr Term make_small(sword_t x) {
    return Term(term_tag::Smallint::create(x));
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
  constexpr sword_t small_get_signed() const {
    return term_tag::Smallint::value(m_val);
  }
  inline word_t small_get_unsigned() const {
    sword_t v = term_tag::Smallint::value(m_val);
    G_ASSERT(v >= 0);
    return (word_t)v;
  }
  inline static bool are_both_small(Term a, Term b) {
    return term_tag::Smallint::check(a.value() & b.value());
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
  // Port id (Oid)
  //
  constexpr bool is_short_port() const {
    return term_tag::ShortPort::check(m_val);
  }
  constexpr bool is_port() const {
    return is_short_port() || term_tag::BoxedPort::check(m_val);
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
    ((word_t *)elements)[0] = arity;
    return Term(term_tag::Tuple::create_from_ptr(elements));
  }
  constexpr bool is_tuple() const {
    return term_tag::Tuple::check(m_val);
  }
  inline word_t tuple_get_arity() const {
    auto p = boxed_get_ptr<word_t>();
    return p[0];
  }
  // Zero based index n
  inline Term tuple_get_element(word_t n) const {
    auto p = boxed_get_ptr<word_t>();
    G_ASSERT(p[0] > n);
    return Term(p[n+1]);
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
  void print();
  void println();
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
};

static_assert(sizeof(Term) == sizeof(word_t),
              "Term size should be same as machine word");

#if G_TEST
void term_test(int argc, const char *argv[]);
#endif // TEST

class Process;
typedef Term (*gc_bif0_fn)(Process *);
typedef Term (*gc_bif1_fn)(Process *, Term);
typedef Term (*gc_bif2_fn)(Process *, Term, Term);


} // ns gluon
