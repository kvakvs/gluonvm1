#include "g_codeserver.h"
#include "g_vm.h"
#include "g_sys_mem.h"
#include "g_reader.h"
#include "g_ext_term.h"

namespace gluon {

class LoaderState {
public:
  Vector<Str>     m_atoms;
  UniquePtr<u8_t> m_code;
  Vector<Term>    m_literals;

  void load_atom_table(tool::Reader &r);
  void load_fun_table(tool::Reader &r) {
    auto sz = r.read_var<word_t>();
    G_ASSERT(sz == 0);
  }
  void load_export_table(tool::Reader &r) {
    auto sz = r.read_var<word_t>();
    G_ASSERT(sz == 0);
  }
  void load_code(tool::Reader &r);
  void load_literal_table(tool::Reader &r);
};

void CodeServer::load_module(Term name_atom, const u8_t *bytes, word_t size) {
  G_ASSERT(name_atom.is_atom() || name_atom.is_nil());
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

  while (1) {
    Str chunk = r.read_string(4);
    if (chunk == "Atom") { lstate.load_atom_table(r); }
    else if (chunk == "FunT") { lstate.load_fun_table(r); }
    else if (chunk == "ExpT") { lstate.load_export_table(r); }
    else if (chunk == "Code") { lstate.load_code(r); }
    else if (chunk == "LitT") { lstate.load_literal_table(r); }
  }
}

void LoaderState::load_atom_table(tool::Reader &r)
{
  auto bytes_sz = r.read_var<word_t>();
  r.assert_remaining_at_least(bytes_sz);

  auto tab_sz = r.read_var<word_t>();
  m_atoms.reserve(tab_sz);
  for (auto i = 0; i < tab_sz; ++i) {
    auto atom_sz = r.read_var<word_t>();
    m_atoms.push_back(r.read_string(atom_sz));
  }
}

void LoaderState::load_code(tool::Reader &r) {
  auto sz = r.read_var<word_t>();
  r.assert_remaining_at_least(sz);

  auto dptr = mem::alloc_bytes(sz).get_result(); // TODO: feeling lucky
  m_code.reset(dptr);
  r.read_bytes(dptr, sz);
}

void LoaderState::load_literal_table(tool::Reader &r)
{
  auto all_sz = r.read_var<word_t>();
  r.assert_remaining_at_least(all_sz);

  Heap *tmp_heap = nullptr;

  auto count = r.read_var<word_t>();
  m_literals.reserve(count);

  for (auto i = 0; i < count; ++i) {
    auto lit_sz = r.read_var<word_t>();
    Term lit = etf::read_ext_term(tmp_heap, r);
    m_literals.push_back(lit);
  }
}

} // ns gluon
