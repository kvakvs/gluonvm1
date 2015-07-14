#pragma once

#include "g_defs.h"

namespace gluon {

namespace term_tag {

  const word_t PRIMARY_SIZE = 2;
  const word_t PRIMARY_MASK = 0x3;
//  const word_t PRIMARY_HEADER = 0x0;
//  const word_t PRIMARY_LIST   = 0x1;
//  const word_t PRIMARY_BOXED  = 0x2;
//  const word_t PRIMARY_IMMED1 = 0x3;

//  constexpr word_t primary_tag(word_t x) {
//    return x & PRIMARY_MASK;
//  }

  const word_t IMMED1_SIZE   = 4;
  const word_t IMMED1_MASK   = 0xF;
//  const word_t IMMED1_PID    = (0x0 << PRIMARY_SIZE) | PRIMARY_IMMED1;
//  const word_t IMMED1_PORT   = (0x1 << PRIMARY_SIZE) | PRIMARY_IMMED1;
//  const word_t IMMED1_IMMED2 = (0x2 << PRIMARY_SIZE) | PRIMARY_IMMED1;
//  const word_t IMMED1_SMALL  = (0x3 << PRIMARY_SIZE) | PRIMARY_IMMED1;

  const word_t IMMED2_SIZE  = 6;
  const word_t IMMED2_MASK  = 0x3F;
//  const word_t IMMED2_ATOM  = (0x0 << IMMED1_SIZE) | IMMED1_IMMED2;
//  const word_t IMMED2_CATCH = (0x1 << IMMED1_SIZE) | IMMED1_IMMED2;
//  const word_t IMMED2_NIL   = (0x3 << IMMED1_SIZE) | IMMED1_IMMED2;

  enum {
    PRIMARY_HEADER = 0,
    PRIMARY_LIST   = 1,
    PRIMARY_BOXED  = 2,
    PRIMARY_IMMED1 = 3
  };

  enum {
    IMMED1_PID    = (0 << PRIMARY_SIZE) | PRIMARY_IMMED1,
    IMMED1_PORT   = (1 << PRIMARY_SIZE) | PRIMARY_IMMED1,
    IMMED1_IMMED2 = (2 << PRIMARY_SIZE) | PRIMARY_IMMED1,
    IMMED1_SMALL  = (3 << PRIMARY_SIZE) | PRIMARY_IMMED1,
  };

  enum {
    IMMED2_ATOM  = (0 << IMMED1_SIZE) | IMMED1_IMMED2,
    IMMED2_CATCH = (1 << IMMED1_SIZE) | IMMED1_IMMED2,
    IMMED2_NIL   = (3 << IMMED1_SIZE) | IMMED1_IMMED2,
  };

  // HEADER representation:
  // aaaaaaaaaaaaaaaaaaaaaaaaaatttt00  arity:26, tag:4

  // HEADER tags:
  // 0000  ARITYVAL
  // 0001  BINARY_AGGREGATE                       |
  // 001x  BIGNUM with sign bit                   |
  // 0100  REF                                    |
  // 0101  FUN                                    | THINGS
  // 0110  FLONUM                                 |
  // 0111  EXPORT                                 |
  // 1000  REFC_BINARY |                          |
  // 1001  HEAP_BINARY | BINARIES                 |
  // 1010  SUB_BINARY  |                          |
  // 1011    Not used; see comment below
  // 1100    EXTERNAL_PID  |                          |
  // 1101    EXTERNAL_PORT | EXTERNAL THINGS          |
  // 1110    EXTERNAL_REF  |            tag_atom      |
  // 1111    MAP

  // COMMENTS:
  // - The tag is zero for arityval and non-zero for thing headers.
  // - A single bit differentiates between positive and negative bignums.
  // - If more tags are needed, the REF and and EXTERNAL_REF tags could probably
  //   be combined to one tag.
  const word_t ARITYVAL_SUBTAG       = 0x0 << PRIMARY_SIZE; // TUPLE
  const word_t BIN_MATCHSTATE_SUBTAG = 0x1 << PRIMARY_SIZE;
  const word_t POS_BIG_SUBTAG        = 0x2 << PRIMARY_SIZE; // BIG: tags 2&3
  const word_t NEG_BIG_SUBTAG        = 0x3 << PRIMARY_SIZE; // BIG: tags 2&3
  const word_t BIG_SIGN_BIT          = 0x1 << PRIMARY_SIZE;
  const word_t REF_SUBTAG            = 0x4 << PRIMARY_SIZE; // REF
  const word_t FUN_SUBTAG            = 0x5 << PRIMARY_SIZE; // FUN
  const word_t FLOAT_SUBTAG          = 0x6 << PRIMARY_SIZE; // FLOAT
  const word_t EXPORT_SUBTAG         = 0x7 << PRIMARY_SIZE; // FLOAT
  const word_t BINARY_XXX_MASK       = 0x3 << PRIMARY_SIZE;
  const word_t REFC_BINARY_SUBTAG    = 0x8 << PRIMARY_SIZE; // BINARY
  const word_t HEAP_BINARY_SUBTAG    = 0x9 << PRIMARY_SIZE; // BINARY
  const word_t SUB_BINARY_SUBTAG     = 0xA << PRIMARY_SIZE; // BINARY
  // _BINARY_XXX_MASK depends on 0xB being unused
  const word_t EXTERNAL_PID_SUBTAG   = 0xC << PRIMARY_SIZE; // EXTERNAL_PID
  const word_t EXTERNAL_PORT_SUBTAG  = 0xD << PRIMARY_SIZE; // EXTERNAL_PORT
  const word_t EXTERNAL_REF_SUBTAG   = 0xE << PRIMARY_SIZE; // EXTERNAL_REF
  const word_t MAP_SUBTAG            = 0xF << PRIMARY_SIZE; // MAP

  const word_t HEADER_ARITYVAL  = PRIMARY_HEADER|ARITYVAL_SUBTAG;
  const word_t HEADER_FUN       = PRIMARY_HEADER|FUN_SUBTAG;
  const word_t HEADER_POS_BIG   = PRIMARY_HEADER|POS_BIG_SUBTAG;
  const word_t HEADER_NEG_BIG   = PRIMARY_HEADER|NEG_BIG_SUBTAG;
  const word_t HEADER_FLOAT     = PRIMARY_HEADER|FLOAT_SUBTAG;
  const word_t HEADER_EXPORT    = PRIMARY_HEADER|EXPORT_SUBTAG;
  const word_t HEADER_REF       = PRIMARY_HEADER|REF_SUBTAG;
  const word_t HEADER_REFC_BIN  = PRIMARY_HEADER|REFC_BINARY_SUBTAG;
  const word_t HEADER_HEAP_BIN  = PRIMARY_HEADER|HEAP_BINARY_SUBTAG;
  const word_t HEADER_SUB_BIN   = PRIMARY_HEADER|SUB_BINARY_SUBTAG;
  const word_t HEADER_EXTERNAL_PID   = PRIMARY_HEADER|EXTERNAL_PID_SUBTAG;
  const word_t HEADER_EXTERNAL_PORT  = PRIMARY_HEADER|EXTERNAL_PORT_SUBTAG;
  const word_t HEADER_EXTERNAL_REF   = PRIMARY_HEADER|EXTERNAL_REF_SUBTAG;
  const word_t HEADER_BIN_MATCHSTATE = PRIMARY_HEADER|BIN_MATCHSTATE_SUBTAG;
  const word_t HEADER_MAP            = PRIMARY_HEADER|MAP_SUBTAG;


  const word_t HEADER_MASK        = 0x3F;
  const word_t HEADER_SUBTAG_MASK = 0x3C;  // 4 bits for subtag
  const word_t HEADER_ARITY_OFFS  = 6;

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

namespace pid {
  /*
   *  PID layout (internal pids):
   *
   *   |3 3 2 2 2 2 2 2|2 2 2 2 1 1 1 1|1 1 1 1 1 1    |               |
   *   |1 0 9 8 7 6 5 4|3 2 1 0 9 8 7 6|5 4 3 2 1 0 9 8|7 6 5 4 3 2 1 0|
   *   |               |               |               |               |
   *   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   *   |n n n n n n n n n n n n n n n n n n n n n n n n n n n n|0 0|1 1|
   *   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   *
   *  n : number
   *
   *  Old pid layout:
   *
   *   |3 3 2 2 2 2 2 2|2 2 2 2 1 1 1 1|1 1 1 1 1 1    |               |
   *   |1 0 9 8 7 6 5 4|3 2 1 0 9 8 7 6|5 4 3 2 1 0 9 8|7 6 5 4 3 2 1 0|
   *   |               |               |               |               |
   *   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   *   |s s s|n n n n n n n n n n n n n n n|N N N N N N N N|c c|0 0|1 1|
   *   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   *
   *  s : serial
   *  n : number
   *  c : creation
   *  N : node number
   *
   */

  static const word_t PID_NUM_SIZE = 15;

  static const word_t PID_DATA_SIZE = 28;
  static const word_t PID_DATA_SHIFT = (term_tag::IMMED1_SIZE);
  static const word_t PID_SER_SIZE = (PID_DATA_SIZE - PID_NUM_SIZE);

//  #define _GET_PID_DATA(X)  _GETBITS((X),_PID_DATA_SHIFT,_PID_DATA_SIZE)
//  #define _GET_PID_NUM(X)   _GETBITS((X),0,_PID_NUM_SIZE)
//  #define _GET_PID_SER(X)   _GETBITS((X),_PID_NUM_SIZE,_PID_SER_SIZE)

//  #define is_internal_pid(x)  (((x) & _TAG_IMMED1_MASK) == _TAG_IMMED1_PID)
//  #define is_not_internal_pid(x)  (!is_internal_pid((x)))

//  #define _unchecked_internal_pid_node(x) erts_this_node
  //_ET_DECLARE_CHECKED(struct erl_node_ *, internal_pid_node, Eterm)
//  #define internal_pid_node(x) _ET_APPLY(internal_pid_node,(x))

//  #define internal_pid_data_words(x) (1)

  static const word_t ERTS_PTAB_ID_DATA_SIZE = 28;
  static const word_t ERTS_PTAB_ID_DATA_SHIFT = term_tag::IMMED1_SIZE;
  // ERTS_PTAB_MAX_SIZE must be a power of 2
  static const word_t ERTS_PTAB_MAX_SIZE = ((word_t)1) << 27;
//#if (ERTS_PTAB_MAX_SIZE-1) > MAX_SMALL
//# error "The maximum number of processes/ports must fit in a SMALL."
//#endif

//  typedef union {
//#if G_HARDWARE_BITS == 64
//    u64_t val;
//    struct { u32_t a, b; };
//    using half_t = u32_t;
//#elif G_HARDWARE_BITS == 32
//    u32_t val;
//    struct { u16_t a, b; };
//    using half_t = u16_t;
//#endif

//#if G_BIGENDIAN
//    inline void high_half(half_t x) { a = x; }
//    inline void low_half(half_t x) { b = x; }
//#else
//    inline void high_half(half_t x) { b = x; }
//    inline void low_half(half_t x) { a = x; }
//#endif

//  } half_word_t;
} // ns pid

// This wrap is here to make strong type difference between hardware hw::Word
// (which is just a machine size unsigned integer) and term type, which is
// complex bitfield structure.
// Note: Wrapping an integer with class is efficient if all members are inline
// and optimizations are on.
class Term {
private:
  word_t m_val;

public:
  constexpr Term(word_t v): m_val(v) {}
  constexpr Term(): m_val(0) {}
  constexpr Term(const Term &other): m_val(other.m_val) {}

  static const word_t NIL = (~((word_t)0) << term_tag::IMMED2_SIZE) | term_tag::IMMED2_NIL;
  //static const word_t NIL = term_tag::make_atom(0);
  //static const word_t MAX_ATOM_INDEX = ~(~((word_t) 0) << (sizeof(word_t)*8 - term_tag::IMMED2_SIZE));
  static const word_t NO_VALUE = term_tag::PRIMARY_IMMED1;
  static const word_t THE_NON_VALUE = 0;

  constexpr static Term make_nil() {
    return Term(NIL);
  }
  constexpr static Term make_non_value() {
    return Term(THE_NON_VALUE);
  }

  //
  // Bit/arithmetic/comparisons
  //
  inline void set(word_t x) { m_val = x; }
  inline void set(const Term &t) { m_val = t.m_val; }

  //inline Term operator <<(word_t bits) const { return Term(m_val << bits); }
  inline word_t value() const { return m_val; }
  inline bool operator <(const Term &x) const { return m_val < x.m_val; }
  inline bool operator ==(const Term &x) const { return m_val == x.m_val; }
  inline bool operator ==(const word_t x) const { return m_val == x; }
  inline bool operator !=(const Term &x) const { return m_val != x.m_val; }
  inline bool operator !=(const word_t x) const { return m_val != x; }
  inline bool is_nil() const { return m_val == NIL; }
  inline bool is_non_value() const { return m_val == THE_NON_VALUE; }
  inline bool is_value() const { return m_val != THE_NON_VALUE; }

  //
  // Pointer magic
  //
  template <typename T>
  inline static word_t compress_pointer(T *p) {
    return reinterpret_cast<word_t>(p);
  }
  template <typename T>
  inline static T *expand_pointer(word_t p) {
    return reinterpret_cast<T *>(p);
  }
  template <typename T>
  inline T *expand_pointer() {
    return reinterpret_cast<T *>(m_val);
  }

  inline static Term make_boxed(Term *x) {
    return Term(compress_pointer(x) | term_tag::PRIMARY_BOXED);
  }
  inline bool is_not_boxed() const {
    return m_val & (term_tag::PRIMARY_MASK - term_tag::PRIMARY_BOXED);
  }
  inline bool is_boxed() const { return !is_not_boxed(); }
//  inline static Term compress_pointer(Term *term_ptr) {
//    return Term(reinterpret_cast<word_t>(term_ptr));
//  }
//  inline void set_compress_pointer(Term *term_ptr) {
//    m_val = reinterpret_cast<word_t>(term_ptr);
//  }
//  inline Term *expand_pointer() {
//    return reinterpret_cast<Term *>(m_val);
//  }
//  inline static Term *expand_pointer(word_t x) {
//    return reinterpret_cast<Term *>(x);
//  }

  //
  // Atoms
  //
  constexpr static Term make_atom(word_t x) {
    return Term((word_t)((x << term_tag::IMMED2_SIZE) + term_tag::IMMED2_ATOM));
  }
  inline bool is_atom() const {
    return (m_val & term_tag::IMMED2_MASK) == term_tag::IMMED2_ATOM;
  }
  inline word_t atom_val() const {
    return m_val >> term_tag::IMMED2_SIZE;
  }

  //
  // Small Integer
  //
  static const word_t SMALL_BITS = sizeof(word_t)*8 - 4;
  static const word_t MAX_SMALL = (1L << (SMALL_BITS-1))-1;
  static const word_t MIN_SMALL = -(1L << (SMALL_BITS-1));
  static constexpr Term make_small(word_t x) {
    return Term((x << term_tag::IMMED1_SIZE) + term_tag::IMMED1_SMALL);
  }
  template <typename N>
  static constexpr bool is_big(N n) {
    return n >= MIN_SMALL && n <= MAX_SMALL;
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
  static constexpr word_t make_pid_data(word_t ser, word_t num) {
      return (word_t)(ser << pid::PID_NUM_SIZE | num);
  }
  // Data arg is created with make_pid_data
  static Term make_internal_pid(word_t data) {
    return Term((data << term_tag::IMMED2_SIZE) | term_tag::IMMED1_PID);
    //return erts_ptab_make_id(ptab=&erts_proc, data=D, tag=_TAG_IMMED1_PID);
//    const auto tag = term_tag::IMMED1_PID;
//    pid::half_word_t w;

//    u32_t low_data = (u32_t)data;
//    low_data &= (1 << pid::ERTS_PTAB_ID_DATA_SIZE) - 1;
//    low_data <<= pid::ERTS_PTAB_ID_DATA_SHIFT;

//    w.high_half(erts_ptab_data2pix(ptab, data));
//    w.low_half(low_data | ((u32_t)tag));
//    return Term(w.val);
  }

  //
  // Tuple
  //
  static u32_t g_zero_sized_tuple;
  static constexpr Term make_zero_tuple() {
    return Term(compress_pointer(&g_zero_sized_tuple) | PRIMARY_TAG_TUPLE);
  }
};

} // ns gluon
