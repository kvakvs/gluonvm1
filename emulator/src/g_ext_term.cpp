#include "g_vm.h"
#include "g_ext_term.h"

namespace gluon {
namespace etf {

Term read_atom_string(tool::Reader &r) {
  word_t sz = r.read_bigendian_i16();
  Str atom_str = r.read_string(sz);
  return VM::to_atom(atom_str);
}

Term make_pid(Term node, word_t id, word_t serial, u8_t creation) {
  G_TODO("create pid etf");
#if FEATURE_ERL_DIST
  G_TODO("distribution support pid etf")
#endif
}

Term read_ext_term(Heap *heap, tool::Reader &r)
{
  r.assert_byte(ETF_MARKER);

  u8_t tag = r.read_byte();

  switch (tag) {
  case COMPRESSED:
    // =80; 4 bytes size; compressed data
    G_TODO("compressed etf");

  case SMALL_INTEGER_EXT:
    return Term::make_small(r.read_byte());

  case INTEGER_EXT: {
    // 32-bit integer
    word_t n = r.read_bigendian_i32();
#if (G_HARDWARE_BITS > 32)
    // fits into small_int if platform is x64
    return Term::make_small(n);
#else
#   if FEATURE_BIGNUM
    if (Term::is_big(n)) {
      G_TODO("construct bignum etf");
    } else {
      return Term::make_small(n);
    }
#   else
    // no bignum, and hardware bits not enough: much fail here
    return Term::make_nil();
#   endif
#endif
    } // integer_ext

  case OLD_FLOAT_STRING_EXT: {
#if FEATURE_FLOAT
    G_TODO("parse float string etf");
#else
    return Term::make_nil(); // sorry no floats
#endif
    } // old string float_ext

  case IEEE_FLOAT_EXT: {
#if FEATURE_FLOAT
      G_TODO("make ieee 8byte double etf");
#else
      return Term::make_nil();
#endif
    } // new 8byte double float_ext

  case ATOM_EXT: {
      return read_atom_string(r);
    } // atom_ext

  case REFERENCE_EXT: {
      // format: N atom string, 4byte id, 1byte creation
//      Term node = read_atom_string(r);
//      word_t id = r.read_bigendian_i32();
//      u8_t creation = r.read_byte();
      G_TODO("ref etf");
    } // end reference_ext

  case PORT_EXT: {
      // format: N atom string, 4byte id, 1byte creation
//      Term node = read_atom_string(r);
//      word_t id = r.read_bigendian_i32();
//      u8_t creation = r.read_byte();
      G_TODO("port etf");
    } // end reference_ext

  case PID_EXT: {
      // format: N atom string, 4byte id, 4byte serial, 1byte cre
      Term node = read_atom_string(r);
      word_t id = r.read_bigendian_i32();
      word_t serial = r.read_bigendian_i32();
      u8_t creation = r.read_byte();
      return make_pid(node, id, serial, creation);
    } // end reference_ext

  } // switch tag
} // parse function


} // ns etf
} // ns gluon
