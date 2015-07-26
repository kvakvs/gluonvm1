#include "g_codeserver.h"
#include "g_vm.h"
#include "g_sys_mem.h"
#include "g_reader.h"
#include "g_ext_term.h"
#include "g_heap.h"
#include "g_module.h"

// Generated opcode arity table
#include "g_genop.h"

#define MINIZ_HEADER_FILE_ONLY
#include "miniz/miniz.c"

namespace gluon {

class LoaderState {
public:
  Vector<Str>     m_atoms;
  const u8_t     *m_code; // not owned data
  word_t          m_code_size;

  // Data coming from code chunk
  word_t          m_code_version;
  word_t          m_code_opcode_max;
  word_t          m_code_label_count;
  word_t          m_code_fun_count;

  Vector<Term>    m_literals;
  Module::labels_t  m_labels;
  Module::exports_t m_exports;  // list of {f/arity} sequentially
  Module::funs_t    m_funs;     // map({f/arity} => label_index)

  LoaderState(): m_code(nullptr), m_code_size(0) {
  }

  MaybeError load_atom_table(tool::Reader &r, Term expected_name);
  MaybeError load_str_table(tool::Reader &r);
  MaybeError load_fun_table(tool::Reader &r);
  MaybeError load_export_table(tool::Reader &r);
  MaybeError load_import_table(tool::Reader &r);
  MaybeError load_code(tool::Reader &r);
  MaybeError load_literal_table(Heap *heap, tool::Reader &r);
  MaybeError load_labels(Heap *heap, tool::Reader &r);
  MaybeError load_line_table(tool::Reader &r);

  inline const Str &atom_tab_index_to_str(word_t i) const {
    G_ASSERT(i < m_atoms.size());
    return m_atoms[i];
  }

  // Load finished, create a Module object and inform code server
  Result<Module *> finalize(Term modname);
  // Parse raw code creating jump table with decoded args
  MaybeError beam_prepare_code(Module *m, const u8_t *bytes, word_t sz);
  void get_tag_and_value(Heap *heap, tool::Reader &r, word_t &tag, word_t &val);

protected:
//  MaybeError gleam_resolve_labels(const Vector<word_t> &postponed_labels,
//                                  Vector<word_t> &code);
  Result<Term> parse_value(Heap *, tool::Reader &r);
  MaybeError get_tag_and_value(tool::Reader &r, word_t &tag, word_t &value);
  Result<word_t> get_tag_and_value_2(tool::Reader &r,
                                     word_t len_code,
                                     word_t &tag, word_t &result);


  struct Tag { enum {
    // The following operand types for generic instructions
    // occur in beam files.
    Literal       = 0,    // TAG_u
    Integer       = 1,    // integer
    Atom          = 2,       // TAG_a
    XRegister     = 3,
    YRegister     = 4,
    Label         = 5,      // TAG_f
    Character     = 6,  // TAG_h
    Extended      = 7,          // TAG_z extended (list, float etc)
    Extended_Float         = 0,
    Extended_List          = 1,
    Extended_FloatRegister = 2,
    Extended_AllocList     = 3,
    Extended_Literal       = 4,

    // The following operand types are only used in the loader.
    NIL       = 8,
    NoLabel   = 9,
    Register  = 10,  // TAG_r
    V         = 11,
    L         = 12,
    LiteralRef  = 13, // TAG_q
    Overflow  = 14,   // overflow/bigint
  };};
};

Result<Module *> CodeServer::load_module_internal(Term expected_name,
                                                  const u8_t *bytes,
                                                  word_t size) {
  G_ASSERT(expected_name.is_atom() || expected_name.is_nil());
  tool::Reader r(bytes, size);
  LoaderState lstate;

  r.assert_remaining_at_least(4+4+4);
  Str for1_header = r.read_string(4);
  if (for1_header != "FOR1") {
    return error<Module *>("not iff container");
  }

  word_t file_length = r.read_big_u32();
  r.assert_remaining_at_least(file_length);

  Str beam_header = r.read_string(4);
  if (beam_header != "BEAM") {
    return error<Module *>("not BEAM file");
  }

  Heap *heap = VM::get_heap(VM::HEAP_LOADER_TMP);

  MaybeError result;
  while (1) {
    if (r.get_remaining_count() < 5) break;
    Str chunk = r.read_string(4);
    if ((chunk[0] < 'A' || chunk[0] > 'Z')    // always uppercase
        || (chunk[1] < 'A' || chunk[1] > 'z') // can be upper or lowercase
        || (chunk[2] < 'a' || chunk[2] > 'z')) { // lowercase
      G_LOG("beam offset 0x%zx\n", r.get_ptr() - bytes);
      return error<Module *>("bad beam format");
    }
    G_LOG("BEAM section %s\n", chunk.c_str());

    result.clear();
    if      (chunk == "Atom") { result = lstate.load_atom_table(r, expected_name); }
    else if (chunk == "Code") { result = lstate.load_code(r); }
    else if (chunk == "FunT") { result = lstate.load_fun_table(r); }
    else if (chunk == "ExpT") { result = lstate.load_export_table(r); }
    else if (chunk == "LitT") { result = lstate.load_literal_table(heap, r); }
    // else if (chunk == "LABL") { result = lstate.load_labels(heap, r); }
    else if (chunk == "StrT") { result = lstate.load_str_table(r); }
    else if (chunk == "ImpT") { result = lstate.load_import_table(r); }
    // CInf block
    // Attr block
    // Abst block
    else if (chunk == "Line") { result = lstate.load_line_table(r); }
    else {
      auto chunk_sz = r.read_big_u32();
      r.advance_align<4>(chunk_sz);
    }
    G_RETURN_REWRAP_IF_ERROR(result, Module*)
  }

  // All good, deploy the module!
  Term modname = VM::to_atom(lstate.m_atoms[0]);
  return lstate.finalize(modname);
}

Result<Module *> LoaderState::finalize(Term modname) {
  Heap *heap = VM::get_heap(VM::HEAP_CODE);
  Module *newmod = Heap::alloc_object<Module>(heap, // then go constructor args:
                                              modname, m_funs, m_exports);

  // Atoms are already in VM at this point
  //newmod->m_name = modname;
  //newmod->m_code.move(m_code);

  //return success(newmod);
  auto result = beam_prepare_code(newmod, m_code, m_code_size);
  G_RETURN_REWRAP_IF_ERROR(result, Module*);

  return success(newmod);
}

MaybeError LoaderState::load_atom_table(tool::Reader &r0, Term expected_name)
{
  auto chunk_size = r0.read_big_u32();
  tool::Reader r = r0.clone(chunk_size);

  auto tab_sz = r.read_big_u32();
  m_atoms.reserve(tab_sz);
  for (word_t i = 0; i < tab_sz; ++i) {
    auto atom_sz = r.read_var<word_t>();
    m_atoms.push_back(r.read_string(atom_sz));
    G_LOG("atom: %s\n", m_atoms.back().c_str());
  }

  // Check first atom in table which is module name
  G_ASSERT(m_atoms.size() > 0);
  Term modname = VM::to_atom(m_atoms[0]);
  if (false == expected_name.is_nil()
      && modname != expected_name) {
    return "module name mismatch";
  }

  r0.advance_align<4>(chunk_size);
  return success();
}

MaybeError LoaderState::load_str_table(tool::Reader &r0)
{
  auto chunk_size = r0.read_big_u32();
  //tool::Reader r  = r0.clone(chunk_size);

  r0.advance_align<4>(chunk_size);
  return success();
}

MaybeError LoaderState::load_fun_table(tool::Reader &r0) {
  auto chunk_size = r0.read_big_u32();
  tool::Reader r = r0.clone(chunk_size);

  G_ASSERT(m_atoms.size() > 0);
  word_t count = chunk_size / (6*4);
  Term mod = VM::to_atom(m_atoms[0]);

  for (word_t i = 0; i < count; ++i) {
    auto fun_atom_i = r.read_big_u32();
    auto arity      = r.read_big_u32();
    auto offset     = r.read_big_u32();
    auto index      = r.read_big_u32();
    auto nfree      = r.read_big_u32();
    auto ouniq      = r.read_big_u32();

    if (fun_atom_i > m_atoms.size()) {
      return "funt: atom index too big";
    }
    const Str &f_str = atom_tab_index_to_str(fun_atom_i);

    fun_entry_t fe;
    fe.mod = mod;
    fe.fun = VM::to_atom(f_str);
    fe.arity = r.read_big_u32();
    fe.uniq[0] = offset; // use as temp storage
    fe.uniq[1] = fe.uniq[2] = fe.uniq[3] = 0;
    fe.old_uniq = ouniq;
    fe.old_index = fe.index = index;
    fe.num_free = nfree;
    fe.code     = nullptr; // resolve later from uniq0

    m_funs[fun_arity_t(fe.fun, arity)] = fe;
  }

  r0.advance_align<4>(chunk_size);
  return success();
}

MaybeError LoaderState::load_export_table(tool::Reader &r0) {
  auto chunk_size = r0.read_big_u32();
  tool::Reader r = r0.clone(chunk_size);

  auto count = r.read_big_u32();

  for (word_t i = 0; i < count; ++i) {
    auto f_i   = r.read_big_u32();
    if (f_i > m_atoms.size()) {
      return "expt: atom index too big";
    }
    auto f = VM::to_atom(atom_tab_index_to_str(f_i));

    auto arity = r.read_big_u32();
    auto label = r.read_big_u32();

    m_exports[fun_arity_t(f, arity)] = label_index_t(label);
  }

  r0.advance_align<4>(chunk_size);
  return success();
}

MaybeError LoaderState::load_import_table(tool::Reader &r0)
{
  auto chunk_size = r0.read_big_u32();
  //tool::Reader r  = r0.clone(chunk_size);
  //auto count = r.read_big_u32();

  // Read triplets u32 module_atom_id; u32 method_atom_id; u32 arity

  r0.advance_align<4>(chunk_size);
  return success();
}

MaybeError LoaderState::load_code(tool::Reader &r0) {
  auto chunk_size = r0.read_big_u32();
  tool::Reader r  = r0.clone(chunk_size);

  /*auto info_size =*/ r.read_big_u32(); //=16

  m_code_version   = r.read_big_u32();
  if (m_code_version != 0) {
    return "opcode set version";
  }
  m_code_opcode_max  = r.read_big_u32();
  m_code_label_count = r.read_big_u32();
  m_code_fun_count   = r.read_big_u32();

  // Just read code here, parse later
  m_code = r.get_ptr();
  m_code_size = chunk_size;

  r0.advance_align<4>(chunk_size);
  return success();
}

MaybeError LoaderState::load_literal_table(Heap *heap, tool::Reader &r0)
{
  auto chunk_size = r0.read_big_u32();
  tool::Reader r1 = r0.clone(chunk_size);

  auto uncompressed_size = r1.read_big_u32();
  UniquePtr<u8_t> compressed(Heap::alloc_bytes(heap, chunk_size));
  r1.read_bytes(compressed.get(), chunk_size);

  UniquePtr<u8_t> uncompressed(Heap::alloc_bytes(heap, uncompressed_size));

  auto result = mz_uncompress(uncompressed.get(), &uncompressed_size,
                              compressed.get(), chunk_size);
  if (result != MZ_OK) {
    return "beam LitT error";
  }

  tool::Reader r(uncompressed.get(), uncompressed_size);
  auto count = r.read_big_u32();

  G_LOG("compressed %zu bytes, uncomp %zu bytes, count %zu\n",
         chunk_size, uncompressed_size, count);
  m_literals.reserve(count);

  for (word_t i = 0; i < count; ++i) {
    /*auto lit_sz =*/ r.read_big_u32();

    auto lit_result = etf::read_ext_term_with_marker(heap, r);
    G_RETURN_IF_ERROR(lit_result);

    auto lit = lit_result.get_result();
#if G_DEBUG
    lit.print();puts("");
#endif
    m_literals.push_back(lit);
  }

  r0.advance_align<4>(chunk_size);
  return success();
}

MaybeError LoaderState::load_labels(Heap * /*heap*/, tool::Reader &r0)
{
  auto chunk_size = r0.read_var<word_t>();
//  tool::Reader r = r0.clone(chunk_size);

//  auto count = r.read_var<word_t>();

//  m_labels.reserve(count+1);
//  for (word_t i = 0; i < count; ++i) {
//    m_labels.push_back(code_offset_t::wrap(r.read_var<word_t>()));
//  }

  r0.advance_align<4>(chunk_size);
  return success();
}

MaybeError LoaderState::load_line_table(tool::Reader &r0)
{
  auto chunk_size = r0.read_big_u32();

  // u32 table_version=0
  // u32 flags ignore
  // u32 count in code chunk
  // u32 count of line records
  // u32 count of filenames
  // line_record[] (1-base index)
  // filename[] = u16 length + characters

  r0.advance_align<4>(chunk_size);
  return success();
}

// Scans raw code in bytes:sz, and builds jump table with processed args
MaybeError LoaderState::beam_prepare_code(Module *m,
                                          const u8_t *bytes, word_t sz)
{
  // TODO: use some other heap?
  Heap *heap = VM::get_heap(VM::HEAP_LOADER_TMP);

  tool::Reader r(bytes, sz);

  Vector<word_t> code;
  // rough estimate of what code size would be, vector will grow if needed
  code.reserve(sz * 2);

  G_ASSERT(sizeof(void*) == sizeof(word_t));

  // save references to labels in code and resolve them in second pass
  Vector<word_t> postponed_labels;

  word_t arity;
  word_t op_ptr;
  while (!r.is_end()) {
    // Get opcode info
    word_t opcode = (word_t)r.read_byte();
    G_LOG("[0x%zx]: opcode=0x%zx %s; ", code.size(), opcode,
          genop::opcode_name_map[opcode]);

    if (opcode > genop::MAX_OPCODE) {
//      G_FAIL("opcode too big");
      return "opcode too big";
    }


    if (G_UNLIKELY(opcode == genop::OPCODE_INT_CODE_END)) {
      break;
    }

    // TODO: can optimize code here if we generalize args reading
    // and then handle different special opcode cases

    // line/1 opcode
    if (opcode == genop::OPCODE_LINE) {
      auto a = parse_value(heap, r);
      G_RETURN_IF_ERROR_UNLIKELY(a);
      a.get_result().print();
      goto next_op;
    }

    // label/1 opcode - save offset to labels table
    if (opcode == genop::OPCODE_LABEL) {
      auto pv_result = parse_value(heap, r);
      G_RETURN_IF_ERROR_UNLIKELY(pv_result);
      Term label = pv_result.get_result();
      label.println();
      G_ASSERT(label.is_small());

      word_t l_id = label.small_get_unsigned();
      m_labels[l_id] = (&code.back())+1;
      G_LOG("label %zu offset 0x%zx", l_id, code.size());
      goto next_op;
    }

    if (opcode == genop::OPCODE_FUNC_INFO) {
      auto a = parse_value(heap, r);
      G_RETURN_IF_ERROR(a)
      auto b = parse_value(heap, r);
      G_RETURN_IF_ERROR(b);
      auto c = parse_value(heap, r);
      G_RETURN_IF_ERROR(c);
//      a.get_result().print();
//      printf(",");
//      b.get_result().print();
//      printf(",");
//      c.get_result().println();
      goto next_op;
    }

    // Convert opcode into jump address
    op_ptr = reinterpret_cast<word_t>(VM::g_opcode_labels[opcode]);
    code.push_back(op_ptr);
//    G_LOG("loader: op %s (opcode 0x%zx) ptr 0x%zx\n",
//           genop::opcode_name_map[opcode], opcode, op_ptr);

    arity = genop::arity_map[opcode];
    G_LOG("arity=%zu args=(", arity);

    for (word_t a = 0; a < arity; ++a) {
      auto arg_result = parse_value(heap, r);
      G_RETURN_IF_ERROR_UNLIKELY(arg_result);

      Term arg = arg_result.get_result();
      arg.print();

      // Use runtime value 'Catch' to mark label references
      if (term_tag::Catch::check(arg.value())) {
        postponed_labels.push_back(code.size());
      }

      code.push_back(arg.value());
      printf("; ");
    }
    printf(").");

next_op:
    puts("");
  }

  // TODO: just scan code and resolve in place maybe? don't have to accum labels
//  auto stage2 = gleam_resolve_labels(postponed_labels, code);
//  G_RETURN_IF_ERROR(stage2);

//  m->set_labels(m_labels);
//  m->set_code(code); // give ownership
  return success();
}

Result<word_t> LoaderState::get_tag_and_value_2(tool::Reader &r,
                                                word_t len_code,
                                                word_t &tag, word_t &result)
{
//  uint8_t default_buf[128];
//  uint8_t *bigbuf = default_buf;
//  uint8_t *s;
//  int i;
//  int neg = 0;
//  size_t arity;
//  Eterm *hp;

  // Retrieve the size of the value in bytes

  len_code >>= 5;

  word_t count;

  if (len_code < 7) {
    count = len_code + 2;
  } else {
    word_t sztag;
    word_t len_word;

    G_ASSERT(len_code == 7);
    auto gt_result = get_tag_and_value(r, sztag, len_word);
    G_RETURN_REWRAP_IF_ERROR(gt_result, word_t);
    if (sztag != Tag::Literal) {
      return error<word_t>("literal tag expected");
    }
    count = len_word + 9;
  }

  // The value for tags except Tag::Integer must be an unsigned integer
  // fitting in a word_t. If it does not fit, we'll indicate overflow
  // by changing the tag to Tag::Overflow.
  if (tag != Tag::Integer) {
    if (count == sizeof(word_t) + 1) {
      // The encoded value has one more byte than a word_t.
      // It will still fit in an word_t if the most significant
      // byte is 0.
      word_t msb;
      msb = r.read_byte();
      result = r.read_big<word_t>();

      if (msb != 0) {
        // Overflow: Negative or too big.
        return success((word_t)Tag::Overflow);
      }
    } else if (count == sizeof(word_t)) {
      // The value must be positive (or the encoded value would have been
      // 1 byte longer).
      result = r.read_big<word_t>();

    } else if (count < sizeof(word_t)) {
      result = r.read_big<word_t>(count);
      // If the sign bit is set, the value is negative (not allowed).
      if (result & (word_t)(1UL << (count * 8 - 1))) {
        return success((word_t)Tag::Overflow);
      }

    } else {
      result = r.read_big<word_t>(count);
      return success((word_t)Tag::Overflow);
    }

    return success(tag);
  }

  //
  // Tag::Integer: First handle values up to the size of an word_t (i.e.
  // either a small or a bignum).
  //
  sword_t val;

  if (count <= sizeof(val)) {
    val = r.read_big<sword_t>(count);
    val = ((val << 8 * (sizeof(val) - count)) >> 8 * (sizeof(val) - count));

    if (Term::does_fit_into_small(val)) {
      result = val;
      return success((word_t)Tag::Integer);
    } else {
#if FEATURE_BIGNUM
      *result = new_literal(stp, &hp, BIG_UINT_HEAP_SIZE);
      (void) small_to_big(val, hp);
      return TAG_q;
#else
      G_FAIL("FEATURE_BIGNUM");
#endif
    }
  }

#if FEATURE_BIGNUM
  //
  // Make sure that the number will fit in our temporary buffer
  // (including margin).
  //
  if (count + 8 > sizeof(default_buf)) {
    bigbuf = (uint8_t *)erts_alloc(ERTS_ALC_T_LOADER_TMP, count + 8);
  }

  /*
   * Copy the number reversed to our temporary buffer.
   */

  GetString(stp, s, count);

  for (i = 0; i < count; i++) {
    bigbuf[count - i - 1] = *s++;
  }

  /*
   * Check if the number is negative, and negate it if so.
   */

  if ((bigbuf[count - 1] & 0x80) != 0) {
    unsigned carry = 1;

    neg = 1;

    for (i = 0; i < count; i++) {
      bigbuf[i] = ~bigbuf[i] + carry;
      carry = (bigbuf[i] == 0 && carry == 1);
    }

    ASSERT(carry == 0);
  }

  /*
   * Align to word boundary.
   */

  if (bigbuf[count - 1] == 0) {
    count--;
  }

  if (bigbuf[count - 1] == 0) {
    LoadError0(stp, "bignum not normalized");
  }

  while (count % sizeof(Eterm) != 0) {
    bigbuf[count++] = 0;
  }

  /*
   * Allocate heap space for the bignum and copy it.
   */

  arity = count / sizeof(Eterm);
  *result = new_literal(stp, &hp, arity + 1);

  if (is_nil(bytes_to_big(bigbuf, count, neg, hp))) {
    goto load_error;
  }

  if (bigbuf != default_buf) {
    erts_free(ERTS_ALC_T_LOADER_TMP, (void *) bigbuf);
  }

  return TAG_q;
#else
      G_FAIL("FEATURE_BIGNUM");
#endif // bignum

//load_error:

//  if (bigbuf != default_buf) {
//    erts_free(ERTS_ALC_T_LOADER_TMP, (void *) bigbuf);
//  }

//  return -1;
}

MaybeError LoaderState::get_tag_and_value(tool::Reader &r,
                                          word_t &tag, word_t &value) {
  auto w = r.read_byte();
  tag = w & 0x07;
  if ((w & 0x08) == 0) {
    value = w >> 4;
  } else if ((w & 0x10) == 0) {
    value = (w >> 5) << 8;
    w = r.read_byte();
    value |= w;
  } else {
    auto res2 = get_tag_and_value_2(r, w, tag, value);
    G_RETURN_IF_ERROR_UNLIKELY(res2);
    tag = res2.get_result();
  }
  return success();
}

Result<Term> LoaderState::parse_value(Heap *, tool::Reader &r)
{
  word_t type, val;
  auto gt_result = get_tag_and_value(r, type, val);
  G_RETURN_REWRAP_IF_ERROR_UNLIKELY(gt_result, Term);

  switch (type) {
  case Tag::Integer:
  case Tag::Literal:
    return success(Term::make_small_u(val));

  case Tag::LiteralRef:
    return success(m_literals[val]);

  case Tag::Overflow:
    return error<Term>("overflow");

  case Tag::XRegister:
    if (val >= VM_MAX_REGS) {
      return error<Term>("invalid x register");
    }
    return success(Term::make_regx(val));

  case Tag::YRegister:
    if (val >= VM_MAX_REGS) {
      return error<Term>("invalid y register");
    }
    return success(Term::make_regy(val));

  case Tag::Atom:
    if (val == 0) {
      return success(Term::make_nil());
    } else if (val >= m_atoms.size()) {
      return error<Term>("bad atom index");
    }
    return success(VM::to_atom(m_atoms[val]));

  case Tag::Label:
    if (val == 0) {
      return success(Term::make_non_value()); //Tag::NoLabel; // empty destination
    } else if (val >= m_code_label_count) {
      return error<Term>("bad label");
    }
    // special value to be recognized by label resolver
    return success(Term::make_catch(val));

  case Tag::Character:
    if (val > 65535) {
      return error<Term>("invalid character range");
    }
    return success(Term::make_small_u(val));

  case Tag::Extended: {
    word_t tag;

    switch (val) {
    case Tag::Extended_Float:
#if FEATURE_FLOAT
      /* Floating point number.
      * Not generated by the compiler in R16B and later.
      */
    {
      Eterm *hp;
      /* XXX:PaN - Halfword should use ARCH_64 variant instead */
#if !defined(ARCH_64) || HALFWORD_HEAP
      size_t high, low;
# endif
      last_op->a[arg].val = new_literal(stp, &hp,
                                        FLOAT_SIZE_OBJECT);
      hp[0] = HEADER_FLONUM;
      last_op->a[arg].type = TAG_q;
#if defined(ARCH_64) && !HALFWORD_HEAP
      GetInt(stp, 8, hp[1]);
# else
      GetInt(stp, 4, high);
      GetInt(stp, 4, low);

      if (must_swap_floats) {
        size_t t = high;
        high = low;
        low = t;
      }

      hp[1] = high;
      hp[2] = low;
# endif
    }
      break;
#else
      // NOTE: code not generated after R16B
      return error<Term>("FEATURE_FLOAT");
#endif

    case Tag::Extended_List: {
//      if (arg + 1 != arity) {
//        LoadError0(stp, "list argument must be the last argument");
//      }
      gt_result = get_tag_and_value(r, tag, val);
      G_RETURN_REWRAP_IF_ERROR_UNLIKELY(gt_result, Term);
      if (tag != Tag::Literal) {
        return error<Term>("extlist: literal tag expected");
      }
      type = Tag::Literal;
//      last_op->a = (GenOpArg *)
//                   erts_alloc(ERTS_ALC_T_LOADER_TMP,
//                              (arity + last_op->a[arg].val)
//                              * sizeof(GenOpArg));
//      memcpy(last_op->a, last_op->def_args,
//             arity * sizeof(GenOpArg));
//      arity += last_op->a[arg].val;
      G_FAIL("ext list?");
      //break;
    }

    case Tag::Extended_FloatRegister: {
#if FEATURE_FLOAT
        gt_result = get_tag_and_value(r, tag, val);
        G_RETURN_REWRAP_IF_ERROR_UNLIKELY(gt_result, Term);
        if (tag != Tag::Literal) {
          return error<Term>("fpreg: literal tag expected");
        }
        type = Tag::L;
        break;
#else
        return error<Term>("FEATURE_FLOAT");
#endif
      }

    case Tag::Extended_AllocList: {
      word_t n;
      word_t type1;
      word_t val1;
      word_t words = 0;

      gt_result = get_tag_and_value(r, tag, n);
      G_RETURN_REWRAP_IF_ERROR_UNLIKELY(gt_result, Term);

      if (tag != Tag::Literal) {
        return error<Term>("a_list: literal tag expected");
      }

      while (n-- > 0) {
        gt_result = get_tag_and_value(r, tag, type1);
        if (tag != Tag::Literal) {
          return error<Term>("a_list: literal tag expected");
        }
        gt_result = get_tag_and_value(r, tag, val1);
        if (tag != Tag::Literal) {
          return error<Term>("a_list: literal tag expected");
        }

        switch (type1) {
        case 0: // Heap words
          words += val1;
          break;

        case 1:
#if FEATURE_FLOAT
          words += FLOAT_SIZE_OBJECT * val;
          break;
#else
          return error<Term>("FEATURE_FLOAT");
#endif

        default:
          return error<Term>("a_list: bad allocation descr");
        }
      }

      type = Tag::Literal;
      val = words;
      //break;
      G_FAIL("alloc list?");
    }

    case Tag::Extended_Literal: {
      word_t val1;

      gt_result = get_tag_and_value(r, tag, val1);
      if (tag != Tag::Literal) {
        return error<Term>("lit: lit tag expected");
      }
      if (val1 >= m_literals.size()) {
        return error<Term>("bad literal index");
      }

//      type = Tag::LiteralRef;
      return success(m_literals[val1]);
    }

    default:
      return error<Term>("bad extended tag");
    }
  }

  default:
    return error<Term>("bad tag");
  }
}

/*
MaybeError LoaderState::gleam_resolve_labels(
                                    const Vector<word_t> &postponed_labels,
                                    Vector<word_t> &code)
{
//  word_t *base = code.data();
  for (word_t i = 0; i < postponed_labels.size(); ++i) {
    word_t code_index = postponed_labels[i];

    // Unwrap catch-marked value
    word_t label_index = term_tag::Catch::value(code[code_index]);

    // New value will be small int
    Term resolved_label = Term::make_boxed(m_labels[label_index]);
    G_LOG("loader: resolving label %zu at 0x%zx to 0x%zx\n",
           label_index,
           code_index,
           (word_t)resolved_label.boxed_get_ptr<word_t>());
    code[code_index] = resolved_label.value();
  }
  return success();
}
*/

/*
Term LoaderState::gleam_read_arg_value(Heap *heap, tool::Reader &r)
{
  u8_t tag = r.read_byte();

  // TODO: pack these little better
  const u8_t tag_integer_pos  = 255; // FF
  const u8_t tag_integer_neg  = 254; // FE
  const u8_t tag_atom         = 253; // FD
  const u8_t tag_label        = 252; // FC
//  const u8_t tag_mfarity    = 251; // FB
  const u8_t tag_register     = 250; // FA
  const u8_t tag_stack        = 249; // F9
  const u8_t tag_nil          = 248; // F8
  const u8_t tag_literal      = 247; // F7
#if FEATURE_FLOAT
  const u8_t tag_fp_register  = 246; // F6
#endif
  const u8_t tag_list         = 245; // F5 - tag for select clauses

  switch (tag) {
  case tag_integer_pos:
  case tag_integer_neg: {
      word_t x0 = r.read_var<word_t>();
      sword_t x = (tag == tag_integer_pos? (sword_t)x0 : -(sword_t)x0);
      if (Term::does_fit_into_small(x)) {
        return Term::make_small(x);
      } else {
        G_TODO("int does not fit into small");
      }
      G_IF_NODEBUG(break;)
    }
  case tag_atom: {
      word_t atom_index = r.read_var<word_t>();
      G_ASSERT(atom_index < m_atoms.size());
      return VM::to_atom(atom_tab_index_to_str(atom_index));
    }
  case tag_label: {
      word_t label_index = r.read_var<word_t>();
//      G_ASSERT(label_index < m_labels.size());
//      auto l_offset = m_labels[label_index].value;
//      return Term::make_small((sword_t)l_offset);

      // Use runtime value 'Catch' to mark label references then process them
      // on the second pass
      return Term(term_tag::Catch::create(label_index));
    }
//  case tag_mfarity: {
//      Term_t m = gleam_read_arg_value(heap, r);
//      Term_t f = gleam_read_arg_value(heap, r);
//      Term_t arity = gleam_read_arg_value(heap, r);
//    }
  case tag_register: {
      word_t reg_index = r.read_var<word_t>();
      return Term::make_regx(reg_index);
    }
  case tag_stack: {
      word_t stk_index = r.read_var<word_t>();
      return Term::make_regy(stk_index);
    }
  case tag_nil:
    return Term::make_nil();
  case tag_literal: {
      word_t lit_index = r.read_var<word_t>();
      G_ASSERT(lit_index < m_literals.size());
      G_LOG("referred lit ");
      m_literals[lit_index].println();
      return m_literals[lit_index];
    }
#if FEATURE_FLOAT
  case tag_fp_register: {
      word_t fp_index = r.read_var<word_t>();
      return Term::make_regfp(fp_index);
    }
#endif
  case tag_list: {
      word_t sz = r.read_var<word_t>();
      Term *elts = Heap::alloc<Term>(heap, sz+1);
      for (word_t i = 0; i < sz; ++i) {
        elts[i+1] = gleam_read_arg_value(heap, r);
      }
      return Term::make_tuple(elts, sz);
    }
  default: G_FAIL("strange value tag");
  } // case tag of

  //return Term::make_nil();
}
*/

} // ns gluon
