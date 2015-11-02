#include "g_code_server.h"
#include "platf/gsys_mem.h"
#include "g_reader.h"
#include "g_ext_term.h"
#include "g_heap.h"
#include "g_predef_atoms.h"
#include "g_term_helpers.h"
#include "g_binary.h"
#include "g_functional.h"

#include "g_beam.h"

#define MINIZ_HEADER_FILE_ONLY
#include "miniz/miniz.c"

namespace gluon {

Module* code::Server::load_module_internal(proc::Heap* heap,
                                           Term expected_name,
                                           ArrayView<const Uint8> data) {
  G_ASSERT(expected_name.is_atom() || expected_name.is_nil());
  tool::Reader r(data);

  BeamLoader lstate(vm_, heap);

  r.assert_remaining_at_least(4 + 4 + 4);
  Str for1_header = r.read_string(4);
  if (for1_header != "FOR1") {
    throw err::BeamLoad("not iff container");
  }

  Word file_length = r.read_big_u32();
  r.assert_remaining_at_least(file_length);

  Str beam_header = r.read_string(4);
  if (beam_header != "BEAM") {
    throw err::BeamLoad("not BEAM file");
  }

  while (1) {
    if (r.get_remaining_count() < 5)
      break;
    Str chunk = r.read_string(4);
    if (!is_uppcase_latin(chunk[0]) || !is_latin(chunk[1]) ||
        !is_lowcase_latin(chunk[2])) {
      G_LOG("beam offset " FMT_0xHEX "\n", r.get_ptr() - data.data());
      throw err::BeamLoad("bad beam format");
    }

    if (chunk == "Atom") {
      lstate.load_atom_table(r, expected_name);
    } else if (chunk == "Code") {
      lstate.load_code(r);
    } else if (chunk == "FunT") {
      lstate.load_lambda_table(r);
    } else if (chunk == "ExpT") {
      lstate.load_export_table(r);
    } else if (chunk == "LitT") {
      lstate.load_literal_table(r);
    }
    // else if (chunk == "LABL") { lstate.load_labels(r); }
    else if (chunk == "StrT") {
      lstate.load_str_table(r);
    } else if (chunk == "ImpT") {
      lstate.load_import_table(r);
    }
    // CInf block
    // Attr block
    // Abst block
    else if (chunk == "Line") {
      lstate.load_line_table(r);
    } else {
      auto chunk_sz = r.read_big_u32();
      r.advance_align<4>(chunk_sz);
    }
  }

  // All good, deploy the module!
  Term modname = vm_.to_atom(lstate.mod_name());
  return lstate.finalize(modname);
}

Module* BeamLoader::finalize(Term modname) {
  Module* newmod = heap_->alloc_object<Module>(modname, imports_);

  beam_prepare_code(newmod, code_);
  return newmod;
}

void BeamLoader::load_atom_table(tool::Reader& r0, Term expected_name) {
  auto chunk_size = r0.read_big_u32();
  tool::Reader r = r0.clone(chunk_size);

  auto tab_sz = r.read_big_u32();
  atoms_.reserve(tab_sz);
  for (Word i = 0; i < tab_sz; ++i) {
    auto atom_sz = r.read_byte();
    atoms_.push_back(r.read_string(atom_sz));
    //    G_LOG("atom: %s index " FMT_UWORD "\n", m_atoms.back().c_str(),
    //    m_atoms.size()-1);
  }

  // Check first atom in table which is module name
  G_ASSERT(atoms_.size() > 0);
  mod_name_ = vm_.to_atom(atoms_[0]);
  if (false == expected_name.is_nil() && mod_name_ != expected_name) {
    throw err::BeamLoad("module name mismatch");
  }

  r0.advance_align<4>(chunk_size);
}

void BeamLoader::load_str_table(tool::Reader& r0) {
  auto chunk_size = r0.read_big_u32();
  r0.advance_align<4>(chunk_size);
}

void BeamLoader::load_lambda_table(tool::Reader& r0) {
  auto chunk_size = r0.read_big_u32();
  tool::Reader r = r0.clone(chunk_size);

  G_ASSERT(atoms_.size() > 0);
  Word count = r.read_big_u32();
  Term mod = vm_.to_atom(atoms_[0]);

  for (Word i = 0; i < count; ++i)
  // while (r.get_remaining_count() > 6 * sizeof(u32_t))
  {
    auto fun_atom_i = r.read_big_u32();
    auto arity = r.read_big_u32();
    auto offset = r.read_big_u32();
    auto index = r.read_big_u32();
    auto nfree = r.read_big_u32();
    auto ouniq = r.read_big_u32();

    if (fun_atom_i > atoms_.size()) {
      throw err::BeamLoad("funt: atom index too big");
    }
    const Str& f_str = atom_tab_index_to_str(fun_atom_i);

    FunEntry fe;
    fe.mfa = MFArity(mod, vm_.to_atom(f_str), arity);
    fe.uniq[0] = (Uint32)offset;  // use as temp storage, cast down to u32
    fe.uniq[1] = fe.uniq[2] = fe.uniq[3] = 0;
    fe.old_uniq = ouniq;
    fe.old_index = fe.index = index;
    fe.num_free = nfree;
    fe.code = nullptr;  // resolve later from uniq0

    Std::fmt("read fun table: %s:%s/" FMT_UWORD " offset=" FMT_UWORD "\n",
             fe.mfa.mod.atom_str(vm_).c_str(), fe.mfa.fun.atom_str(vm_).c_str(),
             arity, offset);
    lambdas_.push_back(fe);
  }

  r0.advance_align<4>(chunk_size);
}

void BeamLoader::load_export_table(tool::Reader& r0) {
  auto chunk_size = r0.read_big_u32();
  tool::Reader r = r0.clone(chunk_size);

  auto count = r.read_big_u32();

  for (Word i = 0; i < count; ++i) {
    auto f_i = r.read_big_u32();
    if (f_i > atoms_.size()) {
      throw err::BeamLoad("expt: atom index too big");
    }
    auto f = vm_.to_atom(atom_tab_index_to_str(f_i));

    auto arity = r.read_big_u32();
    auto label = r.read_big_u32();

    exp_indexes_.insert(FunArity(f, arity), LabelIndex(label));
  }

  r0.advance_align<4>(chunk_size);
}

void BeamLoader::load_import_table(tool::Reader& r0) {
  auto chunk_size = r0.read_big_u32();
  tool::Reader r = r0.clone(chunk_size);
  auto count = r.read_big_u32();

  // Read triplets u32 module_atom_id; u32 method_atom_id; u32 arity
  imports_.reserve(count);
  for (Word i = 0; i < count; ++i) {
    Str ms = atom_tab_index_to_str(r.read_big_u32());
    Str fs = atom_tab_index_to_str(r.read_big_u32());
    Term m = vm_.to_atom(ms);
    Term f = vm_.to_atom(fs);
    Word arity = r.read_big_u32();
    imports_.push_back(MFArity(m, f, arity));
  }

  r0.advance_align<4>(chunk_size);
}

void BeamLoader::load_code(tool::Reader& r0) {
  auto chunk_size = r0.read_big_u32();
  tool::Reader r = r0.clone(chunk_size);

  /*auto info_size =*/r.read_big_u32();  //=16

  code_ver_ = r.read_big_u32();
  if (code_ver_ != 0) {
    throw err::BeamLoad("opcode set version");
  }
  code_opcode_max = r.read_big_u32();
  code_label_count_ = r.read_big_u32();
  code_fun_count_ = r.read_big_u32();

  if (feature_line_numbers) {
    linenums_.fun_code_map.reserve(code_fun_count_);
  }

  // Just read code here, parse later
  code_ = ArrayView<const Uint8>(r.get_ptr(), chunk_size);

  r0.advance_align<4>(chunk_size);
}

void BeamLoader::load_literal_table(tool::Reader& r0) {
  auto chunk_size = r0.read_big_u32();
  tool::Reader r1 = r0.clone(chunk_size);

  auto uncompressed_size = r1.read_big_u32();
  Term compressed = Term::make_binary(vm_, heap_, chunk_size);
  r1.read_bytes(compressed.binary_get<Uint8>(), chunk_size);

  Term uncompressed = Term::make_binary(vm_, heap_, uncompressed_size);
  auto result =
      mz_uncompress(uncompressed.binary_get<Uint8>(), &uncompressed_size,
                    compressed.binary_get<Uint8>(), chunk_size);
  if (result != MZ_OK) {
    throw err::BeamLoad("beam LitT error");
  }

  tool::Reader r(ArrayView<const Uint8>(uncompressed.binary_get<Uint8>(),
                                        uncompressed_size));
  auto count = r.read_big_u32();

  literals_.reserve(count);

  for (Word i = 0; i < count; ++i) {
    /*auto lit_sz =*/r.read_big_u32();

    auto lit = etf::read_ext_term_with_marker(vm_, heap_, r);
    literals_.push_back(lit);
  }

  r0.advance_align<4>(chunk_size);
}

void BeamLoader::load_labels(tool::Reader& r0) {
  auto chunk_size = r0.read_big_u32();

  r0.advance_align<4>(chunk_size);
}

void BeamLoader::load_line_table(tool::Reader& r0) {
  auto chunk_size = r0.read_big_u32();

  if (!feature_line_numbers) {
    r0.advance_align<4>(chunk_size);
    return;
  }

  tool::Reader r = r0.clone(chunk_size);

  // u32 table_version=0
  if (r.read_big_u32() != 0) {
    // Wrong version. Silently ignore the line number chunk.
    throw err::BeamLoad("bad line info ver");
  }

  r.advance(4);  // flags, ignore
  Word line_instr_count = r.read_big_u32();
  linenums_.line_instr.reserve(line_instr_count);

  linenums_.num_line_refs = r.read_big_u32();
  linenums_.num_filenames = r.read_big_u32();

  // line_record[] (1-base index)
  // invalid location goes as index 0
  linenums_.line_refs.push_back(line::invalid_location);

  Word fname_index = 0;
  // First elements of ref table contain only offsets assuming they are for
  // file #0
  for (Word i = 0; i < linenums_.num_line_refs; ++i) {
    Term val = parse_term(r);
    if (val.is_small()) {
      // We've got an offset for current filename
      Word offs = val.small_word();
      if (G_LIKELY(line::is_valid_loc(fname_index, offs))) {
        linenums_.line_refs.push_back(line::make_location(fname_index, offs));
        // Std::fmt("line info: offs=" FMT_UWORD " f=" FMT_UWORD "\n", offs,
        // fname_index);
      } else {
        linenums_.line_refs.push_back(line::invalid_location);
        Std::fmt("line info: invalid loc\n");
      }
    } else if (val.is_atom()) {
      // reference to another file
      Word a_id = val.atom_val();
      if (a_id > linenums_.num_filenames) {
        throw err::BeamLoad("line info: bad file index");
      }
      fname_index = a_id;
    }
  }

  // filenames[] = u16 length + characters
  for (Word i = 0; i < linenums_.num_filenames; ++i) {
    Word name_sz = r.read_big_u16();
    Str name = r.read_string(name_sz);
    Std::fmt("line info: file %s\n", name.c_str());
    linenums_.filenames.push_back(vm_.to_atom(name));
  }

  r0.advance_align<4>(chunk_size);
}

Term BeamLoader::parse_term(tool::Reader& r) {
  Uint8 first = r.read_byte();
  TermTag tag = peek_tag(first);

  //
  // Base tagged values (int, literal, atom, label, register, character)
  //
  if (is_base_tag(tag)) {
    auto int_overflow = parse_small_int(r, first);
    G_ASSERT(int_overflow.second == false);  // assert there's no overflow
    // TODO: on overflow read bigint or something
    Word val = (Word)int_overflow.first;

    switch (tag) {
      case TermTag::Integer:
      case TermTag::Literal: {
        return Term::make_small((SWord)val);
      }

      case TermTag::Atom: {
        if (val == 0) {
          return the_nil;
        } else if (val > atoms_.size()) {  // we will subtract 1 when indexing
          throw err::BeamLoad("bad atom index");
        }
        return vm_.to_atom(atom_tab_index_to_str(val));
      }

      case TermTag::Label: {
        if (val == 0) {
          return the_non_value;  // Tag::NoLabel; // empty destination
        } else if (val >= code_label_count_) {
          throw err::BeamLoad("bad label");
        }
        // special value to be recognized by label resolver
        return Term::make_catch(val);
      }

      case TermTag::XRegister: {
        if (val >= erts::max_regs) {
          throw err::BeamLoad("invalid x register");
        }
        return Term::make_regx(val);
      }

      case TermTag::YRegister: {
        if (val >= erts::max_stack) {
          throw err::BeamLoad("invalid y register");
        }
        return Term::make_regy(val);
      }

      case TermTag::Character: {
        if (val > 65535) {
          throw err::BeamLoad("invalid character range");
        }
        return Term::make_small_u(val);
      }

      case TermTag::Extended:
      case TermTag::Extended_Base:
      case TermTag::Extended_List:
      case TermTag::Extended_FloatRegister:
      case TermTag::Extended_Literal:
      case TermTag::Extended_AllocList:
        throw err::BeamLoad("bad tag");
    }  // end switch tag (Base tags)
  } else
      // ---- END BASE TAGS --- BEGIN EXTENDED ---
      //
      if (tag == TermTag::Extended_Float) {
    return parse_float(r);
  } else if (tag == TermTag::Extended_List) {
    auto psi = parse_small_int(r, r.read_byte());
    G_ASSERT(psi.second == false);  // assert no overflow
    Word length = (Word)psi.first;
    length /= 2;
    term::TupleBuilder tb(heap_, length * 2);

    for (Word i = 0; i < length; ++i) {
      Term base = parse_term(r);
      tb.add(base);

      auto label = parse_small_int(r, r.read_byte());
      G_ASSERT(label.second == false);  // assert no overflow
      tb.add(Term::make_small(label.first));
    }
    Term result = tb.make_tuple();
    // replace label numbers with pointers when loading finished
    resolve_selectlists_.push_back(result);
    return result;
  } else if (tag == TermTag::Extended_FloatRegister) {
#if FEATURE_FLOAT
    //      pc, val = self._parse_floatreg(pc)
    //      args.append((tag, val))
    gt_result = get_tag_and_value(r, tag, val);
    G_RETURN_REWRAP_IF_ERROR_UNLIKELY(gt_result, Term);
    if (tag != Tag::Literal) {
      return error<Term>("fpreg: literal tag expected");
    }
    type = Tag::L;
    break;
#else
    throw err::FeatureMissing("FLOAT");
#endif
  } else if (tag == TermTag::Extended_AllocList) {
    return parse_alloclist(r);
  } else if (tag == TermTag::Extended_Literal) {
    Word val1 = parse_int_term(r, r.read_byte()).small_word();

    if (val1 >= literals_.size()) {
      throw err::BeamLoad("bad literal index");
    }
    return literals_[val1];
  }
  throw err::BeamLoad("bad extended tag");
}

// TODO: return reader as arg here, parse tag and value at once, while reading
// more
BeamLoader::TermTag BeamLoader::peek_tag(Uint8 value) {
  if ((Word)(value & 0x7) == (Word)TermTag::Extended) {
    Word result = ((Word)value >> 4) + (Word)TermTag::Extended_Base;
    return (TermTag)result;
  }
  return (TermTag)(value & 0x7);
}

Term BeamLoader::parse_int_term(tool::Reader& r, Uint8 first) {
  TermTag tag = peek_tag(first);
  G_ASSERT(tag < TermTag::Extended_Base);
  return create_int_term(r, first);
}

Term BeamLoader::create_int_term(tool::Reader& r, Uint8 tag) {
  if (tag & 0x08) {                // xxxx1xxx
    if (tag & 0x10) {              // xxx11xxx - extended
      if ((tag & 0xe0) == 0xe0) {  // length encoded in the tag
        auto tmp = parse_small_int(r, tag);
        G_ASSERT(tmp.second == false);  // assert no overflow
        auto length = tmp.first + (tag >> 5) + 2;
        return parse_bigint(r, length);
      } else {
        return parse_bigint(r, 2 + (tag >> 5));
      }
    } else {
      auto w = r.read_byte();
      Term result = Term::make_small(((SWord)(tag & 0xe0) << 3) | w);
      return result;
    }
  } else {
    return Term::make_small(tag >> 4);
  }
}

Pair<SWord, bool> BeamLoader::parse_small_int(tool::Reader& r, Uint8 first) {
  TermTag tag = peek_tag(first);
  G_ASSERT(tag < TermTag::Extended_Base);
  return parse_create_small_int(r, first);
}

// Returns pair<result, overflow>
Pair<SWord, bool> BeamLoader::read_signed_word(tool::Reader& r, Word count) {
  if (count == sizeof(Word) + 1) {
    // The encoded value has one more byte than an size_t. It will still fit
    // in an size_t if the most significant byte is 0.
    Uint8 msb = r.read_byte();
    SWord result = r.read_big_s(4);
    return std::make_pair(result,
                          msb == 0);  // caller decides if overflow is ok
  }
  if (count == sizeof(Word)) {
    // The value must be positive (or the encoded value would have been
    // one byte longer)
    return std::make_pair(r.read_big_s(4), false);
  }
  if (count < sizeof(Word)) {
    // If the sign bit is set, the value is negative (not allowed).
    bool overflow = (r.peek_byte() & ((size_t)1 << (count * 8 - 1)));
    return std::make_pair(r.read_big_s(count), overflow);
  }
  throw err::TODO("oops");
}

Pair<SWord, bool> BeamLoader::parse_create_small_int(tool::Reader& r,
                                                     Uint8 tag) {
  if (tag & 0x08) {    // xxxx1xxx
    if (tag & 0x10) {  // xxx11xxx - extended
      Word len_code = tag >> 5;
      if (len_code < 7) {
        Word count = len_code + 2;
        G_ASSERT(count <= sizeof(Word));
        return read_signed_word(r, count);
      }
      // Read int which will encode length - we skip it here, it is too big
      // G_FAIL("smallint too big");
      return std::make_pair(0L, true);
    } else {
      auto w = r.read_byte();
      return std::make_pair(((SWord)(tag & 0xe0) << 3) | w, false);
    }
  }
  return std::make_pair(tag >> 4, false);
}

Term BeamLoader::parse_bigint(tool::Reader&, Word) {
#if FEATURE_BIGNUM
#error "todo load bignum from reader"
#else
  throw err::FeatureMissing("BIGINT");
#endif
}

Term BeamLoader::parse_float(tool::Reader&) {
#if FEATURE_FLOAT
#error "todo load float from reader"
#else
  throw err::FeatureMissing("FLOAT");
#endif
}

Term BeamLoader::parse_alloclist(tool::Reader& r) {
#if 0
  Word n;
  Word type1;
  Word val1;
  Word words = 0;

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
#endif
  throw err::TODO("alloc list?");
}


}  // ns gluon
