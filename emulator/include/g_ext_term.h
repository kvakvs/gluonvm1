#pragma once

#include "g_defs.h"
#include "g_term.h"
#include "g_reader.h"
#include "g_error.h"

namespace gluon {
  class Heap;

namespace etf {


  const u8_t ETF_MARKER = 131;
  enum {
    DIST_HEADER           = 68, // contains atom cache
    IEEE_FLOAT_EXT        = 70, // 8-byte double
    BIT_BINARY_EXT        = 77,
    COMPRESSED            = 80,
    ATOM_CACHE_REF        = 82, // used with dist header
    SMALL_INTEGER_EXT     = 97, // 8bit integer
    INTEGER_EXT           = 98, // 32bit big endian integer
    OLD_FLOAT_STRING_EXT  = 99, // superceded by ieee_float_ext
    ATOM_EXT        = 100,      // atom as string
    REFERENCE_EXT   = 101,      // encoded make_ref()
    PORT_EXT        = 102,      // port, similar to ref()
    PID_EXT         = 103,
    SMALL_TUPLE_EXT = 104,
    LARGE_TUPLE_EXT = 105,
    NIL_EXT         = 106,      // empty list []
    STRING_EXT      = 107,      // 16bit size + bytes
    LIST_EXT        = 108,      // 32bit length, elements, tail (or nil)
    BINARY_EXT      = 109,
    SMALL_BIG_EXT   = 110,
    LARGE_BIG_EXT   = 111,
    SMALL_ATOM_EXT      = 115,
    MAP_EXT             = 116,
    ATOM_UTF8_EXT       = 118,
    SMALL_ATOM_UTF8_EXT = 119,
  };

  // Term will be parsed and stored on heap (reads byte=131 first as an ETF tag)
  Result<Term> read_ext_term(Heap *heap, tool::Reader &r);
  // Term will be parsed and stored on heap (reads type tag first)
  Result<Term> read_ext_term2(Heap *heap, tool::Reader &r);

} // ns etf
} // ns gluon