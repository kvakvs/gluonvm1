#include "g_code.h"
#include "g_reader.h"

#include "g_genop.h"

namespace gluon {

// Scans raw code in bytes:sz, and builds jump table with processed args
MaybeError Code::from_raw_gleam(const u8_t *bytes, word_t sz)
{
  tool::Reader r(bytes, sz);
  // rough estimate of what code size would be, vector will auto expand
  m_code.reserve(sz * 2);

  while (!r.is_end()) {
    // Get opcode info
    word_t opcode = (word_t)r.read_byte();
    if (opcode > genop::MAX_OPCODE) {
      return "opcode too big";
    }

    m_code.push_back(opcode);

    for (word_t a = 0; a < genop::arity_map[opcode]; ++a) {
      //Term arg = read_arg_value(r);
    }
  }

  return success();
}

Term Code::read_arg_value(tool::Reader &r)
{
  u8_t tag = r.read_byte();

  // TODO: pack these little better
  const u8_t tag_integer_pos = 255;
  const u8_t tag_integer_neg = 254;
  const u8_t tag_atom = 253;
  const u8_t tag_label = 252;
  const u8_t tag_mfarity = 251;
  const u8_t tag_register = 250;
  const u8_t tag_stack = 249;
  const u8_t tag_nil = 248;
  const u8_t tag_literal = 247;
  const u8_t tag_fp_register = 246;

  switch (tag) {
  case tag_integer_pos: {
      word_t x = r.read_var<word_t>();
      if (Term::does_fit_into_small(x)) {
        return Term::make_small(x);
      } else {
        G_TODO("posint does not fit into small");
      }
    }
  case tag_integer_neg: {
    G_TODO("negint not supported?")
    }
  case tag_atom: break;
  case tag_label: break;
  case tag_mfarity: break;
  case tag_register: break;
  case tag_stack: break;
  case tag_nil: return Term::make_nil();
  case tag_literal: break;
  case tag_fp_register: break;
  }

  return Term::make_nil();
}

} // ns gluon
