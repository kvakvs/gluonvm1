#pragma once

#include "defs.h"
#include "term.h"
#include "reader.h"
#include "error.h"

namespace gluon {
namespace proc {
class Heap;
}

namespace etf {

constexpr Uint8 etf_marker = 131;
enum class Tag : Uint8 {
  DistHeader = 68,  // contains atom cache
  // 69
  IeeeFloatExt = 70,  // 8-byte double
  // ...
  BitBinaryExt = 77,
  // ...
  Compressed = 80,
  // 81
  AtomCacheRef = 82,  // used with dist header
  // ...
  SmallIntegerExt = 97,    // 8bit integer
  IntegerExt = 98,         // 32bit big endian integer
  OldFloatStringExt = 99,  // superceded by ieee_float_ext
  AtomExt = 100,           // atom as string
  ReferenceExt = 101,      // encoded make_ref()
  PortExt = 102,           // port, similar to ref()
  PidExt = 103,
  SmallTupleExt = 104,
  LargeTupleExt = 105,
  NilExt = 106,     // empty list []
  StringExt = 107,  // 16bit size + bytes
  ListExt = 108,    // 32bit length, elements, tail (or nil)
  BinaryExt = 109,
  SmallBigExt = 110,
  LargeBigExt = 111,
  // NEW_FUN_EXT = 112,
  // EXPORT_EXT = 113,
  // NEW_REFERENCE_EXT = 114,
  SmallAtomExt = 115,
  MapExt = 116,
  // FUN_EXT = 117,
  AtomUtf8Ext = 118,
  SmallAtomUtf8Ext = 119,
};

// Term will be parsed and stored on heap (reads byte=131 first as an ETF tag)
Term read_ext_term_with_marker(VM& vm, proc::Heap* heap, tool::Reader& r);
// Term will be parsed and stored on heap (reads type tag first)
Term read_ext_term(VM& vm, proc::Heap* heap, tool::Reader& r);

}  // ns etf
}  // ns gluon
