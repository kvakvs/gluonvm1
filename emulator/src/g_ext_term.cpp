#include "g_vm.h"
#include "g_ext_term.h"
#include "g_heap.h"

namespace gluon {
namespace etf {

Term read_atom_string_i16(tool::Reader &r);
Term read_atom_string_i8(tool::Reader &r);
Result<Term> read_tagged_atom_string(tool::Reader &r);
Node *get_node(Term /*sysname*/, dist::creation_t /*creation*/);
Result<Term> make_pid(Term sysname, word_t id, word_t serial, u8_t creation);
Result<Term> read_tuple(proc::Heap *heap, tool::Reader &r, word_t arity);
Term read_string_ext(proc::Heap *heap, tool::Reader &r);
Result<Term> read_list_ext(proc::Heap *heap, tool::Reader &r);


// Reads long atom as string and attempts to create it in atom table.
Term read_atom_string_i16(tool::Reader &r) {
  word_t sz = r.read_big_u16();
  Str atom_str = r.read_string(sz);
  return VM::to_atom(atom_str);
}


// Reads short atom as string and attempts to create it in atom table.
Term read_atom_string_i8(tool::Reader &r) {
  word_t sz = r.read_byte();
  Str atom_str = r.read_string(sz);
  return VM::to_atom(atom_str);
}


// Reads tag byte, then reads long or short atom as string and attempts to
// create it in atom table.
Result<Term> read_tagged_atom_string(tool::Reader &r) {
  u8_t tag = r.read_byte();
  switch (tag) {
    case ATOM_EXT:        return success(read_atom_string_i16(r));
    case SMALL_ATOM_EXT:  return success(read_atom_string_i8(r));
  }
  return error<Term>("etf atom expected");
}


Node *get_node(Term /*sysname*/, dist::creation_t /*creation*/) {
#if FEATURE_ERL_DIST
  G_TODO("distribution support pid etf")
#endif
  return VM::dist_this_node();
}


Result<Term> make_pid(Term sysname, word_t id, word_t serial, u8_t creation) {
  if ( !Term::is_valid_pid_id(id)
    || !Term::is_valid_pid_serial(serial)) {
    return error<Term>("bad pid");
  }
  // TODO: check valid creation
  word_t data = Term::make_pid_data(serial, id);
  auto node = get_node(sysname, creation);

  if (node == VM::dist_this_node()) {
    return success(Term::make_short_pid(data));
  }
#if FEATURE_ERL_DIST
  G_TODO("distribution support pid etf");
#endif
  // distribution disabled, no want remote pids
  return error<Term>("FEATURE_ERL_DIST");
}

Result<Term> read_tuple(proc::Heap *heap, tool::Reader &r, word_t arity) {
  if (arity == 0) {
    return success(Term::make_zero_tuple());
  }

  Term *cells = (Term *)heap->h_alloc(layout::TUPLE::box_size(arity));

  // fill elements or die horribly if something does not decode
  for (word_t i = 0; i < arity; ++i) {
    auto elem_result = read_ext_term(heap, r);
    if (elem_result.is_error()) {
      return elem_result;
    }
    layout::TUPLE::element(cells, i) = elem_result.get_result();
  }

  return success(Term::make_tuple(cells, arity));
}


Result<Term> read_ext_term_with_marker(proc::Heap *heap, tool::Reader &r) {
  r.assert_byte(ETF_MARKER);
  return read_ext_term(heap, r);
}


#if FEATURE_MAPS
Term read_map(Heap *heap, tool::Reader &r) {
  word_t arity = r.read_bigendian_i32();

  if (arity == 0) {
    return Term::make_zero_map();
  }

  for (auto i = 0; i < arity; ++i) {
    Term key = read_ext_term2(heap, r);
    if (key.is_non_value()) {
      return key;
    }

    Term val = read_ext_term2(heap, r);
    if (val.is_non_value()) {
      return val;
    }
  }
}
#endif // FEATURE_MAPS


Term read_string_ext(proc::Heap *heap, tool::Reader &r) {
  word_t length = r.read_big_u16();
  if (length == 0) {
    return NIL;
  }

  Term result = NIL;
  Term *ref = &result;

  for (word_t i = 0; i < length; ++i) {
    Term *cons = (Term *)heap->h_alloc(layout::CONS::BOX_SIZE);
    layout::CONS::head(cons) = Term::make_small(r.read_byte());
    *ref = Term::make_cons(cons);
    ref = &layout::CONS::tail(cons);
  }

  *ref = NIL;
  return result;
}

Result<Term> read_list_ext(proc::Heap *heap, tool::Reader &r) {
  word_t length = r.read_big_u32();

  Term result = NIL;
  Term *ref = &result;

  for (sword_t i = (sword_t)length - 1; i >= 0; i--) {
    Term *cons = (Term *)heap->h_alloc(layout::CONS::BOX_SIZE);

    auto v_result = read_ext_term(heap, r);
    if (v_result.is_error()) {
      return v_result;
    }

    layout::CONS::head(cons) = v_result.get_result();
    *ref = Term::make_cons(cons);
    ref = &layout::CONS::tail(cons);
  }

  auto tail_result = read_ext_term(heap, r);
  if (tail_result.is_error()) {
    return tail_result;
  }
  *ref = tail_result.get_result();

  return success(result);
}


Result<Term> read_ext_term(proc::Heap *heap, tool::Reader &r) {
  auto t = r.read_byte();
  switch (t) {
  case COMPRESSED:
    // =80; 4 bytes size; compressed data
    G_TODO("compressed etf");
    G_IF_NODEBUG(break;)

  case SMALL_INTEGER_EXT:
    return success(Term::make_small(r.read_byte()));

  case INTEGER_EXT: {
      // 32-bit integer
      sword_t n = r.read_big_s(4);
      if (get_hardware_bits() > 32) {
        // fits into small_int if platform is x64
        return success(Term::make_small(n));
      } else { // hardware bits = 32
#if FEATURE_BIGNUM
      if (Term::is_big(n)) {
        G_TODO("construct bignum etf");
      } else {
        return Term::make_small(n);
      }
#else
      // no bignum, and hardware bits not enough: much fail here
      return error<Term>("FEATURE_BIGNUM");
#endif
      } // hardware bits = 32
    } // integer_ext

#if FEATURE_FLOAT
  case OLD_FLOAT_STRING_EXT: {
    G_TODO("parse float string etf");
    } // old string float_ext
  case IEEE_FLOAT_EXT: {
      G_TODO("make ieee 8byte double etf");
    } // new 8byte double float_ext
#else
  case OLD_FLOAT_STRING_EXT:
  case IEEE_FLOAT_EXT:
    return error<Term>("FEATURE_FLOAT");
#endif

  case ATOM_UTF8_EXT:   // fall through
  case ATOM_EXT:        return success(read_atom_string_i16(r));
  case SMALL_ATOM_UTF8_EXT: // fall through
  case SMALL_ATOM_EXT:  return success(read_atom_string_i8(r));

  case REFERENCE_EXT: {
      // format: N atom string, 4byte id, 1byte creation
//      Term node = read_atom_string(r);
//      word_t id = r.read_bigendian_i32();
//      u8_t creation = r.read_byte();
      G_TODO("ref etf");
      G_IF_NODEBUG(break;)
    } // end reference_ext

  case PORT_EXT: {
      // format: N atom string, 4byte id, 1byte creation
//      Term node = read_atom_string(r);
//      word_t id = r.read_bigendian_i32();
//      u8_t creation = r.read_byte();
      G_TODO("port etf");
      G_IF_NODEBUG(break;)
    } // end reference_ext

  case PID_EXT: {
      // format: N atom string, 4byte id, 4byte serial, 1byte cre
      auto node_result = read_tagged_atom_string(r);
      G_RETURN_REWRAP_IF_ERROR(node_result, Term);

      Term node     = node_result.get_result();
      word_t id     = r.read_big_u32();
      word_t serial = r.read_big_u32();
      u8_t creation = r.read_byte();
      return make_pid(node, id, serial, creation);
    } // end reference_ext

  case SMALL_TUPLE_EXT: return read_tuple(heap, r, r.read_byte());
  case LARGE_TUPLE_EXT: return read_tuple(heap, r, r.read_big_u32());

  case MAP_EXT:
#if FEATURE_MAPS
    return read_map(heap, r);
#else
    return error<Term>("FEATURE_MAPS");
#endif

  case NIL_EXT:     return success(NIL);
  case STRING_EXT:  return success(read_string_ext(heap, r));
  case LIST_EXT:    return read_list_ext(heap, r);

#if FEATURE_BINARIES
  case BINARY_EXT:  G_TODO("read binary etf");
  case BIT_BINARY_EXT:  G_TODO("read bit-binary etf");
#else
  case BINARY_EXT:
  case BIT_BINARY_EXT: return error<Term>("FEATURE_BINARIES");
#endif

#if FEATURE_BIGNUM
  case SMALL_BIG_EXT: G_TODO("read small-big etf");
  case LARGE_BIG_EXT: G_TODO("read large-big etf");
#else
  case SMALL_BIG_EXT:
  case LARGE_BIG_EXT: return error<Term>("FEATURE_BIGNUM");
#endif

  default:
    Std::fmt("invalid ETF value tag %d\n", t);
    return error<Term>("bad etf tag");
  } // switch tag
} // parse function


} // ns etf
} // ns gluon
