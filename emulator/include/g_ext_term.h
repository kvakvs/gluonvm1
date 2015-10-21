#pragma once

#include "g_defs.h"
#include "g_term.h"
#include "g_reader.h"
#include "g_error.h"

namespace gluon {
namespace proc { class Heap; }

namespace etf {

  constexpr u8_t ETF_MARKER = 131;
  enum {
    DIST_HEADER           = 68,  // contains atom cache
    // 69
    IEEE_FLOAT_EXT        = 70,  // 8-byte double
    // ...
    BIT_BINARY_EXT        = 77,
    // ...
    COMPRESSED            = 80,
    // 81
    ATOM_CACHE_REF        = 82,  // used with dist header
    // ...
    SMALL_INTEGER_EXT     = 97,  // 8bit integer
    INTEGER_EXT           = 98,  // 32bit big endian integer
    OLD_FLOAT_STRING_EXT  = 99,  // superceded by ieee_float_ext
    ATOM_EXT              = 100, // atom as string
    REFERENCE_EXT         = 101, // encoded make_ref()
    PORT_EXT              = 102, // port, similar to ref()
    PID_EXT               = 103,
    SMALL_TUPLE_EXT       = 104,
    LARGE_TUPLE_EXT       = 105,
    NIL_EXT               = 106, // empty list []
    STRING_EXT            = 107, // 16bit size + bytes
    LIST_EXT              = 108, // 32bit length, elements, tail (or nil)
    BINARY_EXT            = 109,
    SMALL_BIG_EXT         = 110,
    LARGE_BIG_EXT         = 111,
    // NEW_FUN_EXT = 112,
    // EXPORT_EXT = 113,
    // NEW_REFERENCE_EXT = 114,
    SMALL_ATOM_EXT        = 115,
    MAP_EXT               = 116,
    // FUN_EXT = 117,
    ATOM_UTF8_EXT         = 118,
    SMALL_ATOM_UTF8_EXT   = 119,
  };

  // Term will be parsed and stored on heap (reads byte=131 first as an ETF tag)
  Term read_ext_term_with_marker(VM &vm, proc::Heap *heap, tool::Reader &r);
  // Term will be parsed and stored on heap (reads type tag first)
  Term read_ext_term(VM &vm, proc::Heap *heap, tool::Reader &r);

} // ns etf
} // ns gluon
