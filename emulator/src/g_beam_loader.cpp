#include "g_code_server.h"
#include "g_vm.h"
#include "platf/gsys_mem.h"
#include "g_reader.h"
#include "g_ext_term.h"
#include "g_heap.h"
#include "g_module.h"
#include "g_predef_atoms.h"
#include "g_term_helpers.h"
#include "g_binary.h"

// Generated opcode arity table
#include "g_genop.h"

#define MINIZ_HEADER_FILE_ONLY
#include "miniz/miniz.c"

namespace gluon {


#if FEATURE_LINE_NUMBERS
typedef struct {
  word_t pos;
  word_t *code_pos;
} line_instr_t;
#endif

class LoaderState {
private:
  VM        &vm_;

  proc::Heap     *heap_= nullptr;
  Vector<Str>     atoms_;
  array_view<const u8_t> code_;
//  const u8_t     *code_ = nullptr; // not owned data
//  word_t          code_sz;
  Term            mod_name_; // an atom in the -module(X) header

  // Data coming from code chunk
  word_t          code_ver_;
  word_t          code_opcode_max;
  word_t          code_label_count_;
  word_t          code_fun_count_;

  Vector<Term>    literals_;
  Module::labels_t  labels_;
  Dict<fun_arity_t, label_index_t> exp_indexes_;  // list of {f/arity} sequentially
  Module::imports_t imports_;
  Module::lambdas_t lambdas_;
  // postponed select list with label numbers. Resolve to code pointers after
  // loading finished
  Vector<Term>    resolve_selectlists_;

#if FEATURE_LINE_NUMBERS
  // Grouped in struct by feature
  struct {
    word_t        num_line_refs;
    word_t        num_filenames;
    Module::line_refs_t   line_refs;
    Module::file_names_t  filenames;

    Vector<line_instr_t>  line_instr;
    // Mapping fun# to code start for it
    Vector<word_t *>      fun_code_map;
    //word_t                m_current_li = 0;
    word_t      fun_id = 0;
  } linenums_;
#endif
#if FEATURE_LINE_NUMBERS || FEATURE_CODE_RANGES
  fun_arity_t current_fun_;
#endif
#if FEATURE_CODE_RANGES
  // Grouped in struct by feature
  struct {
    word_t      *fun_begin;
    // Maps f/arity to code ranges
    code::Index<fun_arity_t> fun_map;
  } coderanges_;
#endif

public:
  LoaderState(VM &vm, proc::Heap *h): vm_(vm), heap_(h) {
  }

  const Str &mod_name() const { return atoms_[0]; }

  void load_atom_table(tool::Reader &r, Term expected_name);
  void load_str_table(tool::Reader &r);
  void load_lambda_table(tool::Reader &r);
  void load_export_table(tool::Reader &r);
  void load_import_table(tool::Reader &r);
  void load_code(tool::Reader &r);
  void load_literal_table(tool::Reader &r);
  void load_labels(tool::Reader &r);
  void load_line_table(tool::Reader &r);

  inline const Str &atom_tab_index_to_str(word_t i) const {
    G_ASSERT(i <= atoms_.size());
    return atoms_[i-1];
  }

  // Load finished, create a Module object and inform code server
  Module *finalize(Term modname);
  // Parse raw code creating jump table with decoded args
  void beam_prepare_code(Module *m, array_view<const u8_t> data);

protected:
  enum class Tag {
    // The following operand types for generic instructions
    // occur in beam files.
    Literal       = 0,    // TAG_u
    Integer       = 1,    // integer
    Atom          = 2,    // TAG_a
    XRegister     = 3,
    YRegister     = 4,
    Label         = 5,    // TAG_f
    Character     = 6,    // TAG_h
    Extended      = 7,    // TAG_z extended (list, float etc)
    Extended_Base          = 8,
    Extended_Float         = 8+0,
    Extended_List          = 8+1, // Select list for 'case X of'
    Extended_FloatRegister = 8+2,
    Extended_AllocList     = 8+3,
    Extended_Literal       = 8+4,

//    // The following operand types are only used in the loader.
//    NIL       = 8,
//    NoLabel   = 9,
//    Register  = 10,  // TAG_r
//    V         = 11,
//    L         = 12,
//    LiteralRef  = 13, // TAG_q
//    Overflow  = 14,   // overflow/bigint
  };

  void resolve_labels(const Vector<word_t> &postponed_labels,
                      Vector<word_t> &code);
//  MaybeError get_tag_and_value(tool::Reader &r, word_t &tag, word_t &value);
//  Result<word_t> get_tag_and_value_2(tool::Reader &r,
//                                     word_t len_code,
//                                     word_t &tag, word_t &result);
  void replace_imp_index_with_ptr(word_t *p, Module *m);
  void replace_lambda_index_with_ptr(word_t *p, Module *m);

#if FEATURE_LINE_NUMBERS
  void beam_op_line(Vector<word_t> &code, Term arg);
#endif
#if FEATURE_LINE_NUMBERS || FEATURE_CODE_RANGES
  void beam_op_func_info(Vector<word_t> &code, Term, Term, Term);
#endif
  Term parse_term(tool::Reader &r);

  static Tag parse_tag(tool::Reader &r, u8_t value, int tag=-1);
  static Term parse_int_term(tool::Reader &r, u8_t first);
  inline static bool is_base_tag(Tag t) { return t < Tag::Extended_Base; }
  static Term create_int_term(tool::Reader &r, u8_t first);
  static Term parse_bigint(tool::Reader & /*r*/, word_t /*byte_count*/);
  static Term parse_float(tool::Reader & /*r*/);
  static Term parse_alloclist(tool::Reader &r);
  static Pair<sword_t, bool> parse_small_int(tool::Reader &r, u8_t first);
  static Pair<sword_t, bool> parse_create_small_int(tool::Reader &r, u8_t tag);
  // Returns result + overflow flag, overflow means we want to read bigint
  static Pair<sword_t, bool> read_signed_word(tool::Reader &r, word_t count);
};

Module *code::Server::load_module_internal(proc::Heap *heap,
                                           Term expected_name,
                                           array_view<u8_t> data)
{
  G_ASSERT(expected_name.is_atom() || expected_name.is_nil());
  tool::Reader r(data);

  LoaderState lstate(vm_, heap);

  r.assert_remaining_at_least(4+4+4);
  Str for1_header = r.read_string(4);
  if (for1_header != "FOR1") {
    throw err::beam_load_error("not iff container");
  }

  word_t file_length = r.read_big_u32();
  r.assert_remaining_at_least(file_length);

  Str beam_header = r.read_string(4);
  if (beam_header != "BEAM") {
    throw err::beam_load_error("not BEAM file");
  }

  while (1) {
    if (r.get_remaining_count() < 5) break;
    Str chunk = r.read_string(4);
    if (!is_uppcase_latin(chunk[0])
        || !is_latin(chunk[1])
        || !is_lowcase_latin(chunk[2]))
    {
      G_LOG("beam offset " FMT_0xHEX "\n", r.get_ptr() - data.data());
      throw err::beam_load_error("bad beam format");
    }

    if      (chunk == "Atom") { lstate.load_atom_table(r, expected_name); }
    else if (chunk == "Code") { lstate.load_code(r); }
    else if (chunk == "FunT") { lstate.load_lambda_table(r); }
    else if (chunk == "ExpT") { lstate.load_export_table(r); }
    else if (chunk == "LitT") { lstate.load_literal_table(r); }
    // else if (chunk == "LABL") { lstate.load_labels(r); }
    else if (chunk == "StrT") { lstate.load_str_table(r); }
    else if (chunk == "ImpT") { lstate.load_import_table(r); }
    // CInf block
    // Attr block
    // Abst block
    else if (chunk == "Line") { lstate.load_line_table(r); }
    else {
      auto chunk_sz = r.read_big_u32();
      r.advance_align<4>(chunk_sz);
    }
  }

  // All good, deploy the module!
  Term modname = vm_.to_atom(lstate.mod_name());
  return lstate.finalize(modname);
}

Module * LoaderState::finalize(Term modname) {
  Module *newmod = heap_->alloc_object<Module>(modname, imports_);

  beam_prepare_code(newmod, code_);
  return newmod;
}

void LoaderState::load_atom_table(tool::Reader &r0, Term expected_name)
{
  auto chunk_size = r0.read_big_u32();
  tool::Reader r = r0.clone(chunk_size);

  auto tab_sz = r.read_big_u32();
  atoms_.reserve(tab_sz);
  for (word_t i = 0; i < tab_sz; ++i) {
    auto atom_sz = r.read_byte();
    atoms_.push_back(r.read_string(atom_sz));
//    G_LOG("atom: %s index " FMT_UWORD "\n", m_atoms.back().c_str(), m_atoms.size()-1);
  }

  // Check first atom in table which is module name
  G_ASSERT(atoms_.size() > 0);
  mod_name_ = vm_.to_atom(atoms_[0]);
  if (false == expected_name.is_nil()
      && mod_name_ != expected_name) {
    throw err::beam_load_error("module name mismatch");
  }

  r0.advance_align<4>(chunk_size);
}

void LoaderState::load_str_table(tool::Reader &r0)
{
  auto chunk_size = r0.read_big_u32();
  r0.advance_align<4>(chunk_size);
}

void LoaderState::load_lambda_table(tool::Reader &r0) {
  auto chunk_size = r0.read_big_u32();
  tool::Reader r = r0.clone(chunk_size);

  G_ASSERT(atoms_.size() > 0);
  word_t count = r.read_big_u32();
  Term mod = vm_.to_atom(atoms_[0]);

  for (word_t i = 0; i < count; ++i)
  //while (r.get_remaining_count() > 6 * sizeof(u32_t))
  {
    auto fun_atom_i = r.read_big_u32();
    auto arity      = r.read_big_u32();
    auto offset     = r.read_big_u32();
    auto index      = r.read_big_u32();
    auto nfree      = r.read_big_u32();
    auto ouniq      = r.read_big_u32();

    if (fun_atom_i > atoms_.size()) {
      throw err::beam_load_error("funt: atom index too big");
    }
    const Str &f_str = atom_tab_index_to_str(fun_atom_i);

    fun_entry_t fe;
    fe.mfa = mfarity_t(mod, vm_.to_atom(f_str), arity);
    fe.uniq[0] = (u32_t)offset; // use as temp storage, cast down to u32
    fe.uniq[1] = fe.uniq[2] = fe.uniq[3] = 0;
    fe.old_uniq = ouniq;
    fe.old_index = fe.index = index;
    fe.num_free = nfree;
    fe.code     = nullptr; // resolve later from uniq0

    Std::fmt("read fun table: %s:%s/" FMT_UWORD " offset=" FMT_UWORD "\n",
           fe.mfa.mod.atom_str(vm_).c_str(),
           fe.mfa.fun.atom_str(vm_).c_str(),
           arity, offset);
    lambdas_.push_back(fe);
  }

  r0.advance_align<4>(chunk_size);
}

void LoaderState::load_export_table(tool::Reader &r0) {
  auto chunk_size = r0.read_big_u32();
  tool::Reader r = r0.clone(chunk_size);

  auto count = r.read_big_u32();

  for (word_t i = 0; i < count; ++i) {
    auto f_i   = r.read_big_u32();
    if (f_i > atoms_.size()) {
      throw err::beam_load_error("expt: atom index too big");
    }
    auto f = vm_.to_atom(atom_tab_index_to_str(f_i));

    auto arity = r.read_big_u32();
    auto label = r.read_big_u32();
//    Std::fmt("load export %s/" FMT_UWORD " @label %zu\n", f.atom_str().c_str(), arity, label);

    exp_indexes_[fun_arity_t(f, arity)] = label_index_t(label);
  }

  r0.advance_align<4>(chunk_size);
}

void LoaderState::load_import_table(tool::Reader &r0)
{
  auto chunk_size = r0.read_big_u32();
  tool::Reader r  = r0.clone(chunk_size);
  auto count = r.read_big_u32();

  // Read triplets u32 module_atom_id; u32 method_atom_id; u32 arity
  imports_.reserve(count);
  for (word_t i = 0; i < count; ++i) {
    Str ms = atom_tab_index_to_str(r.read_big_u32());
    Str fs = atom_tab_index_to_str(r.read_big_u32());
    Term m = vm_.to_atom(ms);
    Term f = vm_.to_atom(fs);
    word_t arity = r.read_big_u32();
    imports_.push_back(mfarity_t(m, f, arity));
  }

  r0.advance_align<4>(chunk_size);
}

void LoaderState::load_code(tool::Reader &r0) {
  auto chunk_size = r0.read_big_u32();
  tool::Reader r  = r0.clone(chunk_size);

  /*auto info_size =*/ r.read_big_u32(); //=16

  code_ver_   = r.read_big_u32();
  if (code_ver_ != 0) {
    throw err::beam_load_error("opcode set version");
  }
  code_opcode_max  = r.read_big_u32();
  code_label_count_ = r.read_big_u32();
  code_fun_count_   = r.read_big_u32();

  if (feature_line_numbers) {
    linenums_.fun_code_map.reserve(code_fun_count_);
  }

  // Just read code here, parse later
  code_ = gsl::as_array_view(r.get_ptr(), chunk_size);

  r0.advance_align<4>(chunk_size);
}

void LoaderState::load_literal_table(tool::Reader &r0)
{
  auto chunk_size = r0.read_big_u32();
  tool::Reader r1 = r0.clone(chunk_size);

  auto uncompressed_size = r1.read_big_u32();
  Term compressed = Term::make_binary(vm_, heap_, chunk_size);
  r1.read_bytes(compressed.binary_get<u8_t>(), chunk_size);

  Term uncompressed = Term::make_binary(vm_, heap_, uncompressed_size);
  auto result = mz_uncompress(uncompressed.binary_get<u8_t>(), &uncompressed_size,
                              compressed.binary_get<u8_t>(), chunk_size);
  if (result != MZ_OK) {
    throw err::beam_load_error("beam LitT error");
  }

  tool::Reader r(
        gsl::as_array_view(uncompressed.binary_get<u8_t>(), uncompressed_size)
        );
  auto count = r.read_big_u32();

  literals_.reserve(count);

  for (word_t i = 0; i < count; ++i) {
    /*auto lit_sz =*/ r.read_big_u32();

    auto lit = etf::read_ext_term_with_marker(vm_, heap_, r);
    literals_.push_back(lit);
  }

  r0.advance_align<4>(chunk_size);
}

void LoaderState::load_labels(tool::Reader &r0)
{
  auto chunk_size = r0.read_big_u32();

  r0.advance_align<4>(chunk_size);
}

void LoaderState::load_line_table(tool::Reader &r0)
{
  auto chunk_size = r0.read_big_u32();

  if (!feature_line_numbers) {
    r0.advance_align<4>(chunk_size);
    return;
  }

  tool::Reader r = r0.clone(chunk_size);

  // u32 table_version=0
  if (r.read_big_u32() != 0) {
  // Wrong version. Silently ignore the line number chunk.
    throw err::beam_load_error("bad line info ver");
  }

  r.advance(4); // flags, ignore
  word_t line_instr_count = r.read_big_u32();
  linenums_.line_instr.reserve(line_instr_count);

  linenums_.num_line_refs      = r.read_big_u32();
  linenums_.num_filenames = r.read_big_u32();

  // line_record[] (1-base index)
  // invalid location goes as index 0
  linenums_.line_refs.push_back(line::invalid_location);

  word_t fname_index = 0;
  // First elements of ref table contain only offsets assuming they are for
  // file #0
  for (word_t i = 0; i < linenums_.num_line_refs; ++i) {
    Term val = parse_term(r);
    if (val.is_small()) {
      // We've got an offset for current filename
      word_t offs = val.small_get_unsigned();
      if (G_LIKELY(line::is_valid_loc(fname_index, offs))) {
        linenums_.line_refs.push_back(line::make_location(fname_index, offs));
        //Std::fmt("line info: offs=" FMT_UWORD " f=" FMT_UWORD "\n", offs, fname_index);
      } else {
        linenums_.line_refs.push_back(line::invalid_location);
        Std::fmt("line info: invalid loc\n");
      }
    } else if (val.is_atom()) {
      // reference to another file
      word_t a_id = val.atom_val();
      if (a_id > linenums_.num_filenames) {
        throw err::beam_load_error("line info: bad file index");
      }
      fname_index = a_id;
    }
  }

  // filenames[] = u16 length + characters
  for (word_t i = 0; i < linenums_.num_filenames; ++i) {
    word_t  name_sz = r.read_big_u16();
    Str     name    = r.read_string(name_sz);
    Std::fmt("line info: file %s\n", name.c_str());
    linenums_.filenames.push_back(vm_.to_atom(name));
  }

  r0.advance_align<4>(chunk_size);
}

// Scans raw code in bytes:sz, and builds jump table with processed args
void LoaderState::beam_prepare_code(Module *m, array_view<const u8_t> data)
{
  // TODO: use some other heap?
  //Heap *heap = VM::get_heap(VM::HEAP_LOADER_TMP);

  tool::Reader r(data);

  Vector<word_t> code;
  // rough estimate of what code size would be, vector will grow if needed
  code.reserve(data.size() * 2);

  G_ASSERT(sizeof(void*) == sizeof(word_t));

  // save references to labels in code and resolve them in second pass
  Vector<word_t> postponed_labels;

  word_t arity;
  word_t op_ptr;
  while (!r.is_end()) {
    // Get opcode info
    word_t opcode = (word_t)r.read_byte();
//    G_LOG("[" FMT_0xHEX "]: opcode=" FMT_0xHEX " %s; ", code.size(), opcode,
//          genop::opcode_name_map[opcode]);

    if (opcode < 1 || opcode > genop::MAX_OPCODE) {
      throw err::beam_load_error("bad opcode");
    }


    if (G_UNLIKELY(opcode == genop::OPCODE_INT_CODE_END)) {
      break;
    }

    // TODO: can optimize code here if we generalize args reading
    // and then handle different special opcode cases

    // line/1 opcode
    if (opcode == genop::OPCODE_LINE) {
      if (feature_line_numbers) {
        beam_op_line(code, parse_term(r));
      }
      continue;
    }

    // label/1 opcode - save offset to labels table
    if (opcode == genop::OPCODE_LABEL) {
      Term label = parse_term(r);
      G_ASSERT(label.is_small());

      word_t l_id = label.small_get_unsigned();
      G_ASSERT(l_id < code_label_count_);
      labels_[l_id] = (&code.back())+1;
      continue;
    }

    if (opcode == genop::OPCODE_FUNC_INFO) {
      tool::Reader r1 = r.clone();
      auto a = parse_term(r1);
      auto b = parse_term(r1);
      auto c = parse_term(r1);
      if (feature_line_numbers || feature_code_ranges) {
        beam_op_func_info(code, a, b, c);
      }
      // FALL THROUGH AND EMIT THE OPCODE
    }

    // Convert opcode into jump address
    op_ptr = reinterpret_cast<word_t>(vm_.g_opcode_labels[opcode]);
    code.push_back(op_ptr);
//    G_LOG("loader: op %s (opcode " FMT_0xHEX ") ptr " FMT_0xHEX "\n",
//           genop::opcode_name_map[opcode], opcode, op_ptr);

    arity = genop::arity_map[opcode];
//    G_LOG("arity=" FMT_UWORD " args=(", arity);

    word_t *first_arg = &code.back() + 1;

    for (word_t a = 0; a < arity; ++a) {
      Term arg = parse_term(r);

      // Use runtime value 'Catch' to mark label references
      if (term_tag::Catch::check(arg.as_word())) {
        postponed_labels.push_back(code.size());
      }

      code.push_back(arg.as_word());
//      Std::fmt("; ");
    }
//    Std::fmt(").\n");

//    // If the command was call_ext to a bif - rewrite with bif call
//    if (opcode == genop::OPCODE_CALL_EXT) {
//      Term a(first_arg[0]);
//      // So, an ext call of arity 1, to something that we have bif for
//      if (a.small_get_unsigned() == 1 && VM::resolve_bif1(mfa) != nullptr) {
//        // replace with bif1 op and rewrite args
//      }
//    }

    // Things to resolve from imports:
    if (opcode == genop::OPCODE_BIF0)
    {
      // bif0 import_index Dst - cannot fail, no fail label
      replace_imp_index_with_ptr(first_arg, m);
    } else if (opcode == genop::OPCODE_BIF1
               || opcode == genop::OPCODE_BIF2
               || opcode == genop::OPCODE_CALL_EXT
               || opcode == genop::OPCODE_CALL_EXT_LAST
               || opcode == genop::OPCODE_CALL_EXT_ONLY)
    {
      // bif1|2 Fail import_index ...Args Dst
      replace_imp_index_with_ptr(first_arg+1, m);
    } else if (opcode == genop::OPCODE_GC_BIF1
               || opcode == genop::OPCODE_GC_BIF2
               || opcode == genop::OPCODE_GC_BIF3)
    {
      // gc_bif1|2|3 Fail Live import_index ...Args Dst
      replace_imp_index_with_ptr(first_arg+2, m);
    } else if (opcode == genop::OPCODE_MAKE_FUN2) {
      // make_fun2 LambdaTableIndex
      replace_lambda_index_with_ptr(first_arg, m);
    }
  } // end for all code

#if FEATURE_CODE_RANGES
  // mark end of code by adding last open fun to code index
  beam_op_func_info(code, NONVALUE, NONVALUE, NONVALUE);
  m->set_fun_ranges(coderanges_.fun_map);
#endif

  // TODO: just scan code and resolve in place maybe? don't have to accum labels
  resolve_labels(postponed_labels, code);

  m->set_code(code); // give ownership

  // Move exports from m_exports to this table, resolving labels to code offsets
  Module::exports_t exports;
//  Std::fmt("exports processing: " FMT_UWORD " items\n", m_exports.size());
  auto exps = exp_indexes_.all();
  for_each_keyvalue(exps, [&](const fun_arity_t &fa, label_index_t lindex) {
                    exports[fa] = export_t(labels_[lindex.value],
                                           mfarity_t(mod_name_, fa));
                  });
  m->set_exports(exports);

  for (auto &la: lambdas_) {
    auto ptr = labels_[la.uniq[0]];
    la.code = ptr;
  }
  m->set_lambdas(lambdas_);

  // Fix select lists (replace label numbers with code pointers)
  for (Term t: resolve_selectlists_) {
    word_t t_arity = t.tuple_get_arity()/2;
    for (word_t i = 0; i < t_arity; ++i) {
      word_t l_index = t.tuple_get_element(i*2+1).small_get_unsigned();
      G_ASSERT(labels_.contains(l_index));
      word_t *ptr = labels_[l_index];
      t.tuple_set_element(i*2+1, Term::make_boxed_cp(ptr));
    }
  }
  m->set_labels(labels_); // transfer data

  // TODO: merge code and literal heap together maybe?

#if FEATURE_LINE_NUMBERS
  m->set_line_numbers(linenums_.line_refs, linenums_.filenames);
#endif
}

void LoaderState::replace_imp_index_with_ptr(word_t *p, Module *m) {
  Term i(*p);
  Term j = Term::make_boxed(m->get_import_entry(i.small_get_unsigned()));
  *p = j.as_word();
}

#if FEATURE_LINE_NUMBERS
void LoaderState::beam_op_line(Vector<word_t> &code, Term arg)
{
  word_t index = arg.small_get_unsigned();
  G_ASSERT(index < linenums_.line_refs.size());

  line_instr_t li;
  li.code_pos = code.data();
  li.pos = linenums_.line_refs[index];
  linenums_.line_instr.push_back(li);
}
#endif

#if FEATURE_LINE_NUMBERS || FEATURE_CODE_RANGES
void LoaderState::beam_op_func_info(Vector<word_t> &code, Term, Term f, Term a)
{
#if FEATURE_CODE_RANGES
  word_t *last_ptr = (&code.back()) + 1;

  // Finish previous function if it already started
  if (current_fun_.fun.is_value()) {
    code::Range range(coderanges_.fun_begin, last_ptr);
//    Std::fmt("fun map: " FMT_0xHEX ".." FMT_0xHEX " %s/" FMT_UWORD "\n", (word_t)CR.fun_begin,
//           (word_t)last_ptr, m_current_fun.first.atom_c_str(),
//           m_current_fun.second);
    coderanges_.fun_map.add(range, current_fun_);
  }
  if (f.is_non_value()) {
    // we mark end of code by calling this with a non-value
    return;
  }
#endif

  // TODO: nif loading goes here
  //m_function_number++;
  //G_ASSERT(m_code_fun_count >= m_function_number);

  current_fun_.fun = f;
  current_fun_.arity = a.small_get_unsigned();

#if FEATURE_CODE_RANGES
  coderanges_.fun_begin = last_ptr;
#endif

#if FEATURE_LINE_NUMBERS
  // save fun start address
  //m_func_line[m_function_number] = code.data() + code.size();
  linenums_.fun_code_map.push_back(code.data() + code.size());
#endif
}
#endif

void LoaderState::replace_lambda_index_with_ptr(word_t *p, Module *m) {
  Term i(*p);
  Term j = Term::make_boxed(&lambdas_[i.small_get_unsigned()]);
  *p = j.as_word();
}

Term LoaderState::parse_term(tool::Reader &r)
{
  u8_t first  = r.read_byte();
  Tag tag     = parse_tag(r, first);

  //
  // Base tagged values (int, literal, atom, label, register, character)
  //
  if (is_base_tag(tag)) {
    auto psi_result = parse_small_int(r, first);
    G_ASSERT(psi_result.second == false); // assert there's no overflow
    // TODO: on overflow read bigint or something
    word_t val = (word_t)psi_result.first;

    if (tag == Tag::Integer || tag == Tag::Literal) {
      return Term::make_small((sword_t)val);
    }
    if (tag == Tag::Atom) {
      if (val == 0) {
        return NIL;
      } else if (val > atoms_.size()) { // we will subtract 1 when indexing
        throw err::beam_load_error("bad atom index");
      }
      return vm_.to_atom(atom_tab_index_to_str(val));
    }
    if (tag == Tag::Label) {
      if (val == 0) {
        return NONVALUE; //Tag::NoLabel; // empty destination
      } else if (val >= code_label_count_) {
        throw err::beam_load_error("bad label");
      }
      // special value to be recognized by label resolver
      return Term::make_catch(val);
    }
    if (tag == Tag::XRegister) {
      if (val >= erts::max_regs) {
        throw err::beam_load_error("invalid x register");
      }
      return Term::make_regx(val);
    }
    if (tag == Tag::YRegister) {
      if (val >= erts::max_stack) {
        throw err::beam_load_error("invalid y register");
      }
      return Term::make_regy(val);
    }
    if (tag == Tag::Character) {
      if (val > 65535) {
        throw err::beam_load_error("invalid character range");
      }
      return Term::make_small_u(val);
    }
  }
  // ---- END BASE TAGS --- BEGIN EXTENDED ---
  //
  else if (tag == Tag::Extended_Float) {
    return parse_float(r);
  }
  else if (tag == Tag::Extended_List) {
    auto psi = parse_small_int(r, r.read_byte());
    G_ASSERT(psi.second == false); // assert no overflow
    word_t length = (word_t)psi.first;
    length /= 2;
    term::TupleBuilder tb(heap_, length * 2);

    for (word_t i = 0; i < length; ++i) {
      Term base = parse_term(r);
      tb.add(base);

      auto label = parse_small_int(r, r.read_byte());
      G_ASSERT(label.second == false); // assert no overflow
      tb.add(Term::make_small(label.first));
    }
    Term result = tb.make_tuple();
    // replace label numbers with pointers when loading finished
    resolve_selectlists_.push_back(result);
    return result;
  }
  else if (tag == Tag::Extended_FloatRegister) {
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
    throw err::feature_missing_error("FLOAT");
#endif
  }
  else if (tag == Tag::Extended_AllocList) {
    return parse_alloclist(r);
  }
  else if (tag == Tag::Extended_Literal) {
    word_t val1 = parse_int_term(r, r.read_byte()).small_get_unsigned();

    if (val1 >= literals_.size()) {
      throw err::beam_load_error("bad literal index");
    }
    return literals_[val1];
  }
  throw err::beam_load_error("bad extended tag");
}

LoaderState::Tag LoaderState::parse_tag(tool::Reader &r, u8_t value, int tag) {
  if (tag == -1) {
    tag = value;
  }
  if ((tag & 0x7) == (u8_t)Tag::Extended) {
    return (Tag)(((u8_t)tag >> 4) + (u8_t)Tag::Extended_Base);
  }
  return (Tag)(tag & 0x7);
}

Term LoaderState::parse_int_term(tool::Reader &r, u8_t first) {
  Tag tag = parse_tag(r, first);
  G_ASSERT(tag < Tag::Extended_Base);
  return create_int_term(r, first);
}

Term LoaderState::create_int_term(tool::Reader &r, u8_t tag)
{
  if (tag & 0x08) {  // xxxx1xxx
    if (tag & 0x10) {  // xxx11xxx - extended
      if ((tag & 0xe0) == 0xe0) { // length encoded in the tag
        auto tmp = parse_small_int(r, tag);
        G_ASSERT(tmp.second == false); // assert no overflow
        auto length = tmp.first + (tag >> 5) + 2;
        return parse_bigint(r, length);
      } else {
        return parse_bigint(r, 2 + (tag >> 5));
      }
    } else {
      auto w = r.read_byte();
      Term result = Term::make_small(((sword_t)(tag & 0xe0) << 3) | w);
      return result;
    }
  } else {
    return Term::make_small(tag >> 4);
  }
}

Pair<sword_t, bool>
LoaderState::parse_small_int(tool::Reader &r, u8_t first) {
  Tag tag = parse_tag(r, first);
  G_ASSERT(tag < Tag::Extended_Base);
  return parse_create_small_int(r, first);
}

// Returns pair<result, overflow>
Pair<sword_t, bool>
LoaderState::read_signed_word(tool::Reader &r, word_t count) {
  if (count == sizeof(word_t) + 1) {
    // The encoded value has one more byte than an size_t. It will still fit
    // in an size_t if the most significant byte is 0.
    u8_t msb = r.read_byte();
    sword_t result = r.read_big_s(4);
    return std::make_pair(result, msb == 0); // caller decides if overflow is ok
  }
  if (count == sizeof(word_t)) {
    // The value must be positive (or the encoded value would have been
    // one byte longer)
    return std::make_pair(r.read_big_s(4), false);
  }
  if (count < sizeof(word_t)) {
    // If the sign bit is set, the value is negative (not allowed).
    bool overflow = (r.peek_byte() & ((size_t)1 << (count * 8 - 1)));
    return std::make_pair(r.read_big_s(count), overflow);
  }
  G_FAIL("oops");
}

Pair<sword_t, bool>
LoaderState::parse_create_small_int(tool::Reader &r, u8_t tag)
{
  if (tag & 0x08) {  // xxxx1xxx
    if (tag & 0x10) {  // xxx11xxx - extended
      word_t len_code = tag >> 5;
      if (len_code < 7) {
        word_t count = len_code + 2;
        G_ASSERT(count <= sizeof(word_t));
        return read_signed_word(r, count);
      }
      // Read int which will encode length - we skip it here, it is too big
      //G_FAIL("smallint too big");
      return std::make_pair(0L, true);
    } else {
      auto w = r.read_byte();
      return std::make_pair(((sword_t)(tag & 0xe0) << 3) | w, false);
    }
  }
  return std::make_pair(tag >> 4, false);
}

Term LoaderState::parse_bigint(tool::Reader &, word_t) {
#if FEATURE_BIGNUM
#error "todo load bignum from reader"
#else
  throw err::feature_missing_error("BIGINT");
#endif
}

Term LoaderState::parse_float(tool::Reader &) {
#if FEATURE_FLOAT
#error "todo load float from reader"
#else
  throw err::feature_missing_error("FLOAT");
#endif
}

Term LoaderState::parse_alloclist(tool::Reader &r)
{
#if 0
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
#endif
  G_FAIL("alloc list?");
}

void LoaderState::resolve_labels(const Vector<word_t> &postponed_labels,
                                       Vector<word_t> &code)
{
  //  word_t *base = code.data();
  for (word_t i = 0; i < postponed_labels.size(); ++i) {
    word_t code_index = postponed_labels[i];

    // Unwrap catch-marked value
    word_t label_index = term_tag::Catch::value(code[code_index]);

    // New value will be small int
    Term resolved_label = Term::make_boxed_cp(labels_[label_index]);
    code[code_index] = resolved_label.as_word();
  }
  return;
}

} // ns gluon
