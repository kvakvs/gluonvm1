#include "g_codeserver.h"
#include "g_vm.h"
#include "g_sys_mem.h"
#include "g_reader.h"
#include "g_ext_term.h"
#include "g_heap.h"
#include "g_module.h"

// Generated opcode arity table
#include "g_genop.h"

namespace gluon {

class LoaderState {
public:
  Vector<Str>     m_atoms;
  const u8_t     *m_code; // not owned data
  word_t          m_code_size;
  Vector<Term>    m_literals;
  Module::labels_t  m_labels;
  Module::exports_t m_exports;  // list of {f/arity} sequentially
  Module::funs_t    m_funs;     // map({f/arity} => label_index)

  LoaderState(): m_code(nullptr), m_code_size(0) {
  }

  MaybeError load_atom_table(tool::Reader &r);
  MaybeError load_fun_table(tool::Reader &r);
  MaybeError load_export_table(tool::Reader &r);
  MaybeError load_code(tool::Reader &r);
  MaybeError load_literal_table(Heap *heap, tool::Reader &r);
  MaybeError load_labels(Heap *heap, tool::Reader &r);

  inline const Str &atom_tab_index_to_str(word_t i) const {
    G_ASSERT(i < m_atoms.size());
    return m_atoms[i];
  }

  // Load finished, create a Module object and inform code server
  Result<Module *> finalize(Term modname);
  // Parse raw code creating jump table with decoded args
  MaybeError gleam_prepare_code(Module *m, const u8_t *bytes, word_t sz);
  // Parse one argument converting it into Term ready to be stored in code
  Term gleam_read_arg_value(Heap *heap, tool::Reader &r);
protected:
  MaybeError gleam_resolve_labels(const Vector<word_t> &postponed_labels,
                                  Vector<word_t> &code);
};

Result<Module *> CodeServer::load_module_internal(Term expected_name_or_nil,
                                                  const u8_t *bytes,
                                                  word_t size) {
  G_ASSERT(expected_name_or_nil.is_atom() || expected_name_or_nil.is_nil());
  tool::Reader r(bytes, size);
  LoaderState lstate;

  const word_t HDR_SIZE = 5;

  // assert there's at least data for 'GLEAM' and 1 letter module name
  r.assert_remaining_at_least(HDR_SIZE+2);
  Str header = r.read_string(HDR_SIZE);
  G_ASSERT(header == "GLEAM");

  auto modname_sz = r.read_byte();
  Str modname_s = r.read_string(modname_sz);
  Term modname = VM::to_atom(modname_s);
  if (false == expected_name_or_nil.is_nil()
      && modname != expected_name_or_nil) {
    return error<Module *>("module name does not match");
  }

  Heap *heap = VM::get_heap(VM::HEAP_LOADER_TMP);

  MaybeError result;
  while (1) {
    if (r.get_remaining_count() < 5) break;
    Str chunk = r.read_string(4);
    G_LOG("GLEAM section %s\n", chunk.c_str());

    result.clear();
    if      (chunk == "ATOM") { result = lstate.load_atom_table(r); }
    else if (chunk == "FUNT") { result = lstate.load_fun_table(r); }
    else if (chunk == "EXPT") { result = lstate.load_export_table(r); }
    else if (chunk == "CODE") { result = lstate.load_code(r); }
    else if (chunk == "LTRL") { result = lstate.load_literal_table(heap, r); }
    else if (chunk == "LABL") { result = lstate.load_labels(heap, r); }
    G_RETURN_REWRAP_IF_ERROR(result, Module*)
  }

  // All good, deploy the module!
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
  auto result = gleam_prepare_code(newmod, m_code, m_code_size);
  G_RETURN_REWRAP_IF_ERROR(result, Module*);

  return success(newmod);
}

MaybeError LoaderState::load_atom_table(tool::Reader &r0)
{
  auto chunk_size = r0.read_var<word_t>();
  tool::Reader r = r0.clone(chunk_size);

  auto tab_sz = r.read_var<word_t>();
  m_atoms.reserve(tab_sz);
  for (word_t i = 0; i < tab_sz; ++i) {
    auto atom_sz = r.read_var<word_t>();
    m_atoms.push_back(r.read_string(atom_sz));
  }

  r0.advance(chunk_size);
  return success();
}

MaybeError LoaderState::load_fun_table(tool::Reader &r0) {
  auto chunk_size = r0.read_var<word_t>();
  tool::Reader r = r0.clone(chunk_size);

  G_ASSERT(m_atoms.size() > 0);
  word_t count = r.read_var<word_t>();
  for (word_t i = 0; i < count; ++i) {
    auto f_i   = r.read_var<word_t>();
    if (f_i > m_atoms.size()) {
      return "funt: atom index too big";
    }
    const Str &f_str = atom_tab_index_to_str(f_i);
    auto f     = VM::to_atom(f_str);

    auto arity = r.read_var<word_t>();
    auto lbl   = r.read_var<word_t>();
//    G_LOG("load_fun_t %s/%zu -> label %zu\n", f_str.c_str(), arity, lbl);
    m_funs[fun_arity_t::create(f, arity)] = label_index_t::wrap(lbl);
  }

  r0.advance(chunk_size);
  return success();
}

MaybeError LoaderState::load_export_table(tool::Reader &r0) {
  auto chunk_size = r0.read_var<word_t>();
  tool::Reader r = r0.clone(chunk_size);

  G_ASSERT(m_atoms.size() > 0);
  word_t count = r.read_var<word_t>();
  for (word_t i = 0; i < count; ++i) {
    auto f_i   = r.read_var<word_t>();
    if (f_i > m_atoms.size()) {
      return "expt: atom index too big";
    }
    auto f = VM::to_atom(atom_tab_index_to_str(f_i));

    auto arity = r.read_var<word_t>();
    m_exports.push_back(fun_arity_t::create(f, arity));
  }

  r0.advance(chunk_size);
  return success();
}

MaybeError LoaderState::load_code(tool::Reader &r0) {
  auto chunk_size = r0.read_var<word_t>();
  tool::Reader r = r0.clone(chunk_size);

  //  G_LOG("code section %zu bytes\n", chunk_size);

  //auto dptr = mem::alloc_bytes(chunk_size).get_result(); // TODO: feeling lucky
  //r.read_bytes(dptr, chunk_size);
  m_code = r.get_ptr();
  m_code_size = chunk_size;

  r0.advance(chunk_size);
  return success();
}

MaybeError LoaderState::load_literal_table(Heap *heap, tool::Reader &r0)
{
//  G_LOG("load lit table\n");
  auto chunk_size = r0.read_var<word_t>();
  tool::Reader r = r0.clone(chunk_size);

  auto count = r.read_var<word_t>();
  m_literals.reserve(count);

  for (word_t i = 0; i < count; ++i) {
    /*auto lit_sz =*/ r.read_var<word_t>();

    auto lit_result = etf::read_ext_term(heap, r);
    G_RETURN_IF_ERROR(lit_result);

    auto lit = lit_result.get_result();
#if G_DEBUG
    lit.print();puts("");
#endif
    m_literals.push_back(lit);
  }

  r0.advance(chunk_size);
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

  r0.advance(chunk_size);
  return success();
}

// Scans raw code in bytes:sz, and builds jump table with processed args
MaybeError LoaderState::gleam_prepare_code(Module *m,
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

  while (!r.is_end()) {
    // Get opcode info
    word_t opcode = (word_t)r.read_byte();
    printf("[%zu]: ", code.size());

    if (opcode > genop::MAX_OPCODE) {
      G_FAIL("opcode too big");
//      return "opcode too big";
    }

    // line/1 opcode
    if (opcode == genop::OPCODE_LINE) {
      gleam_read_arg_value(heap, r);
      continue;
    }

    // label/1 opcode - save offset to labels table
    if (opcode == genop::OPCODE_LABEL) {
      Term label = gleam_read_arg_value(heap, r);
      G_ASSERT(label.is_small());

      word_t l_id = (word_t)label.small_get_value();
      m_labels[l_id] = code.size();
      printf("loader: label %zu\n", l_id);
      continue;
    }

    // Convert opcode into jump address
    word_t op_ptr = reinterpret_cast<word_t>(VM::g_opcode_labels[opcode]);
    code.push_back(op_ptr);
    printf("loader: op %s (opcode %zx) ptr %zx\n",
           genop::opcode_name_map[opcode], opcode, op_ptr);

    word_t arity = genop::arity_map[opcode];
//    printf("opcode 0x%zx %s; arity %zu\n", opcode, genop::opcode_name_map[opcode], arity);

    for (word_t a = 0; a < arity; ++a) {
      Term arg = gleam_read_arg_value(heap, r);

      // Use runtime value 'Catch' to mark label references
      if (term_tag::Catch::check(arg.value())) {
        postponed_labels.push_back(code.size());
      }

      code.push_back(arg.value());
    }
  }

  auto stage2 = gleam_resolve_labels(postponed_labels, code);
  G_RETURN_IF_ERROR(stage2);

  m->set_labels(m_labels);
  m->set_code(code); // give ownership
  return success();
}

MaybeError LoaderState::gleam_resolve_labels(
                                    const Vector<word_t> &postponed_labels,
                                    Vector<word_t> &code)
{
  for (word_t i = 0; i < postponed_labels.size(); ++i) {
    word_t code_index = postponed_labels[i];

    // Unwrap catch-marked value
    word_t label_index = term_tag::Catch::value(code[code_index]);

    // New value will be small int
    Term resolved_label = Term::make_small((sword_t)m_labels[label_index]);
    printf("loader: resolving label %zu at %zu to %zd\n",
           label_index, code_index, resolved_label.small_get_value());
    code[code_index] = resolved_label.value();
  }
  return success();
}

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
  const u8_t tag_fp_register  = 246; // F6

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
      return term_tag::Catch::create(label_index);
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
      return m_literals[lit_index];
    }
  case tag_fp_register: {
      word_t fp_index = r.read_var<word_t>();
      return Term::make_regfp(fp_index);
    }
  } // case tag of

  return Term::make_nil();
}

} // ns gluon
