#include "g_code_server.h"
#include "g_vm.h"
#include "g_sys_mem.h"
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
public:
  proc::Heap     *m_heap;
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
  Map<fun_arity_t, label_index_t> m_exports;  // list of {f/arity} sequentially
  Module::imports_t m_imports;
  Module::lambdas_t m_lambdas;
  // postponed select list with label numbers. Resolve to code pointers after
  // loading finished
  Vector<Term>    m_resolve_select_lists;

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
  } LN;
#endif
#if FEATURE_LINE_NUMBERS || FEATURE_CODE_RANGES
  fun_arity_t m_current_fun;
#endif
#if FEATURE_CODE_RANGES
  // Grouped in struct by feature
  struct {
    word_t      *fun_begin;
    // Maps f/arity to code ranges
    code::Index<fun_arity_t> fun_map;
  } CR;
#endif

  LoaderState(): m_code(nullptr), m_code_size(0) {
    m_current_fun.first = NONVALUE;
  }

  MaybeError load_atom_table(tool::Reader &r, Term expected_name);
  MaybeError load_str_table(tool::Reader &r);
  MaybeError load_lambda_table(tool::Reader &r);
  MaybeError load_export_table(tool::Reader &r);
  MaybeError load_import_table(tool::Reader &r);
  MaybeError load_code(tool::Reader &r);
  MaybeError load_literal_table(tool::Reader &r);
  MaybeError load_labels(tool::Reader &r);
  MaybeError load_line_table(tool::Reader &r);

  inline const Str &atom_tab_index_to_str(word_t i) const {
    G_ASSERT(i <= m_atoms.size());
    return m_atoms[i-1];
  }

  // Load finished, create a Module object and inform code server
  Result<Module *> finalize(Term modname);
  // Parse raw code creating jump table with decoded args
  MaybeError beam_prepare_code(Module *m, const u8_t *bytes, word_t sz);

protected:
  struct Tag { enum {
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
    };};

  MaybeError resolve_labels(const Vector<word_t> &postponed_labels,
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
  Result<Term> parse_term(tool::Reader &r);

  static u8_t parse_tag(tool::Reader &r, u8_t value, int tag=-1);
  static Result<Term> parse_int_term(tool::Reader &r, u8_t first);
  inline static bool is_base_tag(u8_t t) { return t < Tag::Extended_Base; }
  static Result<Term> create_int_term(tool::Reader &r, u8_t first);
  static Result<Term> parse_bigint(tool::Reader & /*r*/, word_t /*byte_count*/);
  static Result<Term> parse_float(tool::Reader & /*r*/);
  static Result<Term> parse_alloclist(tool::Reader &r);
  static Pair<sword_t, bool> parse_small_int(tool::Reader &r, u8_t first);
  static Pair<sword_t, bool> parse_create_small_int(tool::Reader &r, u8_t tag);
  // Returns result + overflow flag, overflow means we want to read bigint
  static Pair<sword_t, bool> read_signed_word(tool::Reader &r, word_t count);
};

Result<Module *> code::Server::load_module_internal(proc::Heap *heap,
    Term expected_name, const u8_t *bytes, word_t size)
{
  G_ASSERT(expected_name.is_atom() || expected_name.is_nil());
  tool::Reader r(bytes, size);

  LoaderState lstate;
  lstate.m_heap = heap;

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
    //G_LOG("BEAM section %s\n", chunk.c_str());

    result.clear();
    if      (chunk == "Atom") { result = lstate.load_atom_table(r, expected_name); }
    else if (chunk == "Code") { result = lstate.load_code(r); }
    else if (chunk == "FunT") { result = lstate.load_lambda_table(r); }
    else if (chunk == "ExpT") { result = lstate.load_export_table(r); }
    else if (chunk == "LitT") { result = lstate.load_literal_table(r); }
    // else if (chunk == "LABL") { result = lstate.load_labels(r); }
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
  Module *newmod = m_heap->h_alloc_object<Module>(modname, m_imports);

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
    auto atom_sz = r.read_byte();
    m_atoms.push_back(r.read_string(atom_sz));
//    G_LOG("atom: %s index %zu\n", m_atoms.back().c_str(), m_atoms.size()-1);
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

MaybeError LoaderState::load_lambda_table(tool::Reader &r0) {
  auto chunk_size = r0.read_big_u32();
  tool::Reader r = r0.clone(chunk_size);

  G_ASSERT(m_atoms.size() > 0);
  word_t count = r.read_big_u32();
  Term mod = VM::to_atom(m_atoms[0]);

  for (word_t i = 0; i < count; ++i)
  //while (r.get_remaining_count() > 6 * sizeof(u32_t))
  {
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
    fe.mfa = mfarity_t(mod, VM::to_atom(f_str), arity);
    fe.uniq[0] = (u32_t)offset; // use as temp storage, cast down to u32
    fe.uniq[1] = fe.uniq[2] = fe.uniq[3] = 0;
    fe.old_uniq = ouniq;
    fe.old_index = fe.index = index;
    fe.num_free = nfree;
    fe.code     = nullptr; // resolve later from uniq0

    printf("read fun table: %s:%s/%zu offset=%zu\n",
           fe.mfa.mod.atom_str().c_str(),
           fe.mfa.fun.atom_str().c_str(),
           arity, offset);
    m_lambdas.push_back(fe);
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
//    printf("load export %s/%zu @label %zu\n", f.atom_str().c_str(), arity, label);

    m_exports[fun_arity_t(f, arity)] = label_index_t(label);
  }

  r0.advance_align<4>(chunk_size);
  return success();
}

MaybeError LoaderState::load_import_table(tool::Reader &r0)
{
  auto chunk_size = r0.read_big_u32();
  tool::Reader r  = r0.clone(chunk_size);
  auto count = r.read_big_u32();

  // Read triplets u32 module_atom_id; u32 method_atom_id; u32 arity
  m_imports.reserve(count);
  for (word_t i = 0; i < count; ++i) {
    Str ms = atom_tab_index_to_str(r.read_big_u32());
    Str fs = atom_tab_index_to_str(r.read_big_u32());
    Term m = VM::to_atom(ms);
    Term f = VM::to_atom(fs);
    word_t arity = r.read_big_u32();
//    printf("import: %s:%s/%zu\n", ms.c_str(), fs.c_str(), arity);
    m_imports.push_back(mfarity_t(m, f, arity));
  }

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

#if FEATURE_LINE_NUMBERS
  LN.fun_code_map.reserve(m_code_fun_count);
#endif

  // Just read code here, parse later
  m_code = r.get_ptr();
  m_code_size = chunk_size;

  r0.advance_align<4>(chunk_size);
  return success();
}

MaybeError LoaderState::load_literal_table(tool::Reader &r0)
{
  auto chunk_size = r0.read_big_u32();
  tool::Reader r1 = r0.clone(chunk_size);

  auto uncompressed_size = r1.read_big_u32();
  Term compressed = Term::make_binary(m_heap, chunk_size);
  r1.read_bytes(compressed.binary_get<u8_t>(), chunk_size);

  Term uncompressed = Term::make_binary(m_heap, uncompressed_size);
  auto result = mz_uncompress(uncompressed.binary_get<u8_t>(), &uncompressed_size,
                              compressed.binary_get<u8_t>(), chunk_size);
  if (result != MZ_OK) {
    return "beam LitT error";
  }

  tool::Reader r(uncompressed.binary_get<u8_t>(), uncompressed_size);
  auto count = r.read_big_u32();

//  G_LOG("compressed %zu bytes, uncomp %zu bytes, count %zu\n",
//         chunk_size, uncompressed_size, count);
  m_literals.reserve(count);

  for (word_t i = 0; i < count; ++i) {
    /*auto lit_sz =*/ r.read_big_u32();

    auto lit_result = etf::read_ext_term_with_marker(m_heap, r);
    G_RETURN_IF_ERROR(lit_result);

    auto lit = lit_result.get_result();
    m_literals.push_back(lit);
  }

  r0.advance_align<4>(chunk_size);
  return success();
}

MaybeError LoaderState::load_labels(tool::Reader &r0)
{
  auto chunk_size = r0.read_big_u32();
//  tool::Reader r = r0.clone(chunk_size);

  r0.advance_align<4>(chunk_size);
  return success();
}

MaybeError LoaderState::load_line_table(tool::Reader &r0)
{
  auto chunk_size = r0.read_big_u32();

#if FEATURE_LINE_NUMBERS
  tool::Reader r = r0.clone(chunk_size);

  // u32 table_version=0
  if (r.read_big_u32() != 0) {
  // Wrong version. Silently ignore the line number chunk.
    return "bad line info ver";
  }

  r.advance(4); // flags, ignore
  word_t line_instr_count = r.read_big_u32();
  LN.line_instr.reserve(line_instr_count);

  LN.num_line_refs      = r.read_big_u32();
  LN.num_filenames = r.read_big_u32();

  // line_record[] (1-base index)
  // invalid location goes as index 0
  LN.line_refs.push_back(line::INVALID_LOC);

  word_t fname_index = 0;
  // First elements of ref table contain only offsets assuming they are for
  // file #0
  for (word_t i = 0; i < LN.num_line_refs; ++i) {
    auto pt_result = parse_term(heap, r);
    G_RETURN_IF_ERROR_UNLIKELY(pt_result);

    Term val = pt_result.get_result();
    if (val.is_small()) {
      // We've got an offset for current filename
      word_t offs = val.small_get_unsigned();
      if (G_LIKELY(line::is_valid_loc(fname_index, offs))) {
        LN.line_refs.push_back(line::make_location(fname_index, offs));
        printf("line info: offs=%zu f=%zu\n", offs, fname_index);
      } else {
        LN.line_refs.push_back(line::INVALID_LOC);
        printf("line info: invalid loc\n");
      }
    } else if (val.is_atom()) {
      // reference to another file
      word_t a_id = val.atom_val();
      if (a_id > LN.num_filenames) {
        return "line info: bad file index";
      }
      fname_index = a_id;
    }
  }

  // filenames[] = u16 length + characters
  for (word_t i = 0; i < LN.num_filenames; ++i) {
    word_t  name_sz = r.read_big_u16();
    Str     name    = r.read_string(name_sz);
    printf("line info: file %s\n", name.c_str());
    LN.filenames.push_back(VM::to_atom(name));
  }

#endif

  r0.advance_align<4>(chunk_size);
  return success();
}

// Scans raw code in bytes:sz, and builds jump table with processed args
MaybeError LoaderState::beam_prepare_code(Module *m,
                                          const u8_t *bytes, word_t sz)
{
  // TODO: use some other heap?
  //Heap *heap = VM::get_heap(VM::HEAP_LOADER_TMP);

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
//    G_LOG("[0x%zx]: opcode=0x%zx %s; ", code.size(), opcode,
//          genop::opcode_name_map[opcode]);

    if (opcode < 1 || opcode > genop::MAX_OPCODE) {
      return "bad opcode";
    }


    if (G_UNLIKELY(opcode == genop::OPCODE_INT_CODE_END)) {
      break;
    }

    // TODO: can optimize code here if we generalize args reading
    // and then handle different special opcode cases

    // line/1 opcode
    if (opcode == genop::OPCODE_LINE) {
      auto a = parse_term(r);
      G_RETURN_IF_ERROR_UNLIKELY(a);
#if FEATURE_LINE_NUMBERS
      beam_op_line(code, a.get_result());
#else
//      a.get_result().println();
#endif
      continue;
    }

    // label/1 opcode - save offset to labels table
    if (opcode == genop::OPCODE_LABEL) {
      auto pv_result = parse_term(r);
      G_RETURN_IF_ERROR_UNLIKELY(pv_result);
      Term label = pv_result.get_result();
//      label.println();
      G_ASSERT(label.is_small());

      word_t l_id = label.small_get_unsigned();
      G_ASSERT(l_id < m_code_label_count);
      m_labels[l_id] = (&code.back())+1;
//      G_LOG("label %zu (0x%zx) offset 0x%zx", l_id, l_id, code.size());
//      puts("");
      continue;
    }

    if (opcode == genop::OPCODE_FUNC_INFO) {
      tool::Reader r1 = r.clone();
      auto a = parse_term(r1);
      G_RETURN_IF_ERROR(a)
      auto b = parse_term(r1);
      G_RETURN_IF_ERROR(b);
      auto c = parse_term(r1);
      G_RETURN_IF_ERROR(c);
#if FEATURE_LINE_NUMBERS || FEATURE_CODE_RANGES
      beam_op_func_info(code,
                        a.get_result(), b.get_result(), c.get_result());
#else
//      puts("");
#endif
      // FALL THROUGH AND EMIT THE OPCODE
      //continue;
    }

//    if (opcode == genop::OPCODE_PUT) {
//      // eliminate opcode and put only arg
//      auto a = parse_term(heap, r);
//      G_RETURN_IF_ERROR(a);
//      code.push_back(a);
//    }

    // Convert opcode into jump address
    op_ptr = reinterpret_cast<word_t>(VM::g_opcode_labels[opcode]);
    code.push_back(op_ptr);
//    G_LOG("loader: op %s (opcode 0x%zx) ptr 0x%zx\n",
//           genop::opcode_name_map[opcode], opcode, op_ptr);

    arity = genop::arity_map[opcode];
//    G_LOG("arity=%zu args=(", arity);

    word_t *first_arg = &code.back() + 1;

    for (word_t a = 0; a < arity; ++a) {
      auto arg_result = parse_term(r);
      G_RETURN_IF_ERROR_UNLIKELY(arg_result);

      Term arg = arg_result.get_result();
//      arg.print();

      // Use runtime value 'Catch' to mark label references
      if (term_tag::Catch::check(arg.as_word())) {
        postponed_labels.push_back(code.size());
      }

      code.push_back(arg.as_word());
//      printf("; ");
    }
//    printf(").\n");

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
  m->set_fun_ranges(CR.fun_map);
#endif

  // TODO: just scan code and resolve in place maybe? don't have to accum labels
  auto stage2 = resolve_labels(postponed_labels, code);
  G_RETURN_IF_ERROR(stage2);

  m->set_labels(m_labels);
  m->set_code(code); // give ownership

  // Move exports from m_exports to this table, resolving labels to code offsets
  Module::exports_t exports;
//  printf("exports processing: %zu items\n", m_exports.size());
  for (auto &e: m_exports) {
    auto ptr = m_labels[e.second.value];
//    printf("label export ptr 0x%zu\n", (word_t)ptr);
    exports[e.first] = ptr;
  }
  m->set_exports(exports);

  for (auto &la: m_lambdas) {
    auto ptr = m_labels[la.uniq[0]];
    la.code = ptr;
  }
  m->set_lambdas(m_lambdas);

  // Fix select lists (replace label numbers with code pointers)
  for (Term t: m_resolve_select_lists) {
    word_t t_arity = t.tuple_get_arity()/2;
    for (word_t i = 0; i < t_arity; ++i) {
      word_t l_index = t.tuple_get_element(i*2+1).small_get_unsigned();
      word_t *ptr = m_labels[l_index];
      t.tuple_set_element(i*2+1, Term::make_boxed(ptr));
    }
  }

  // TODO: merge code and literal heap together maybe?

#if FEATURE_LINE_NUMBERS
  m->set_line_numbers(LN.line_refs, LN.filenames);
#endif
  return success();
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
  G_ASSERT(index < LN.line_refs.size());

  line_instr_t li;
  li.code_pos = code.data();
  li.pos = LN.line_refs[index];
  LN.line_instr.push_back(li);
}
#endif

#if FEATURE_LINE_NUMBERS || FEATURE_CODE_RANGES
void LoaderState::beam_op_func_info(Vector<word_t> &code, Term, Term f, Term a)
{
#if FEATURE_CODE_RANGES
  word_t *last_ptr = (&code.back()) + 1;

  // Finish previous function if it already started
  if (m_current_fun.first.is_value()) {
    code::Range range(CR.fun_begin, last_ptr);
//    printf("fun map: 0x%zx..0x%zx %s/%zu\n", (word_t)CR.fun_begin,
//           (word_t)last_ptr, m_current_fun.first.atom_c_str(),
//           m_current_fun.second);
    CR.fun_map.add(range, m_current_fun);
  }
  if (f.is_non_value()) {
    // we mark end of code by calling this with a non-value
    return;
  }
#endif

  // TODO: nif loading goes here
  //m_function_number++;
  //G_ASSERT(m_code_fun_count >= m_function_number);

  m_current_fun = std::make_pair(f, a.small_get_unsigned());

#if FEATURE_CODE_RANGES
  CR.fun_begin = last_ptr;
#endif

#if FEATURE_LINE_NUMBERS
  // save fun start address
  //m_func_line[m_function_number] = code.data() + code.size();
  LN.fun_code_map.push_back(code.data() + code.size());
#endif
}
#endif

void LoaderState::replace_lambda_index_with_ptr(word_t *p, Module *m) {
  Term i(*p);
  Term j = Term::make_boxed(&m_lambdas[i.small_get_unsigned()]);
  *p = j.as_word();
}

Result<Term> LoaderState::parse_term(tool::Reader &r)
{
  u8_t first = r.read_byte();
  u8_t tag   = parse_tag(r, first);

  //
  // Base tagged values (int, literal, atom, label, register, character)
  //
  if (is_base_tag(tag)) {
    auto psi_result = parse_small_int(r, first);
    G_ASSERT(psi_result.second == false); // assert there's no overflow
    // TODO: on overflow read bigint or something
    word_t val = (word_t)psi_result.first;

    if (tag == Tag::Integer || tag == Tag::Literal) {
      return success(Term::make_small((sword_t)val));
    }
    if (tag == Tag::Atom) {
      if (val == 0) {
        return success(NIL);
      } else if (val > m_atoms.size()) { // we will subtract 1 when indexing
        return error<Term>("bad atom index");
      }
      return success(VM::to_atom(atom_tab_index_to_str(val)));
    }
    if (tag == Tag::Label) {
      if (val == 0) {
        return success(NONVALUE); //Tag::NoLabel; // empty destination
      } else if (val >= m_code_label_count) {
        return error<Term>("bad label");
      }
      // special value to be recognized by label resolver
      return success(Term::make_catch(val));
    }
    if (tag == Tag::XRegister) {
      if (val >= vm::MAX_REGS) {
        return error<Term>("invalid x register");
      }
      return success(Term::make_regx(val));
    }
    if (tag == Tag::YRegister) {
      if (val >= vm::MAX_STACK) {
        return error<Term>("invalid y register");
      }
      return success(Term::make_regy(val));
    }
    if (tag == Tag::Character) {
      if (val > 65535) {
        return error<Term>("invalid character range");
      }
      return success(Term::make_small_u(val));
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
    term::TupleBuilder tb(m_heap, length * 2);

    for (word_t i = 0; i < length; ++i) {
      auto base_result = parse_term(r);
      G_RETURN_REWRAP_IF_ERROR_UNLIKELY(base_result, Term);
      Term base = base_result.get_result();
      tb.add(base);

      auto label = parse_small_int(r, r.read_byte());
      G_ASSERT(label.second == false); // assert no overflow
      tb.add(Term::make_small(label.first));
    }
    Term result = tb.make_tuple();
    // replace label numbers with pointers when loading finished
    m_resolve_select_lists.push_back(result);
    return success(result);
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
    return error<Term>("FEATURE_FLOAT");
#endif
  }
  else if (tag == Tag::Extended_AllocList) {
    return parse_alloclist(r);
  }
  else if (tag == Tag::Extended_Literal) {
    auto lit_result = parse_int_term(r, r.read_byte());
    G_RETURN_REWRAP_IF_ERROR_UNLIKELY(lit_result, Term);
    word_t val1 = lit_result.get_result().small_get_unsigned();

    if (val1 >= m_literals.size()) {
      return error<Term>("bad literal index");
    }
    return success(m_literals[val1]);
  }
  return error<Term>("bad extended tag");
}

u8_t LoaderState::parse_tag(tool::Reader &r, u8_t value, int tag) {
  if (tag == -1) {
    tag = value;
  }
  if ((tag & 0x7) == Tag::Extended) {
    return ((u8_t)tag >> 4) + Tag::Extended_Base;
  }
  return (tag & 0x7);
}

Result<Term> LoaderState::parse_int_term(tool::Reader &r, u8_t first) {
  u8_t tag = parse_tag(r, first);
  G_ASSERT(tag < Tag::Extended_Base);
  return create_int_term(r, first);
}

Result<Term> LoaderState::create_int_term(tool::Reader &r, u8_t tag)
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
      return success(result);
    }
  } else {
    return success(Term::make_small(tag >> 4));
  }
}

Pair<sword_t, bool>
LoaderState::parse_small_int(tool::Reader &r, u8_t first) {
  u8_t tag = parse_tag(r, first);
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

Result<Term> LoaderState::parse_bigint(tool::Reader &, word_t) {
#if FEATURE_BIGNUM
#error "todo load bignum from reader"
#else
  return error<Term>("FEATURE_BIGINT");
#endif
}

Result<Term> LoaderState::parse_float(tool::Reader &) {
#if FEATURE_FLOAT
#error "todo load float from reader"
#else
  return error<Term>("FEATURE_FLOAT");
#endif
}

Result<Term> LoaderState::parse_alloclist(tool::Reader &r)
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

MaybeError LoaderState::resolve_labels(const Vector<word_t> &postponed_labels,
                                       Vector<word_t> &code)
{
  //  word_t *base = code.data();
  for (word_t i = 0; i < postponed_labels.size(); ++i) {
    word_t code_index = postponed_labels[i];

    // Unwrap catch-marked value
    word_t label_index = term_tag::Catch::value(code[code_index]);

    // New value will be small int
    Term resolved_label = Term::make_boxed(m_labels[label_index]);
//    G_LOG("loader: resolving label %zu at 0x%zx to 0x%zx\n",
//           label_index, code_index,
//           (word_t)resolved_label.boxed_get_ptr<word_t>());
    code[code_index] = resolved_label.as_word();
  }
  return success();
}

} // ns gluon
