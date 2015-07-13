#include "g_codeserver.h"
#include "g_vm.h"
#include "g_sys_mem.h"

namespace gluon {

class Reader {
public:
  const u8_t *m_ptr;
  const u8_t *m_limit;

  Reader(const u8_t *ptr, word_t size): m_ptr(ptr), m_limit(ptr+size) {
  }

  inline u8_t read_byte() {
    // TODO: make this runtime error, not assertion
    G_ASSERT(m_ptr < m_limit);
    // FIXME: am i really not having 1 byte overlap here?
    return *m_ptr++;
  }

  // Advance by 1 byte, assert its value equal to 'value'
  void assert_byte(u8_t value) {
    auto b = read_byte();
    G_ASSERT(value == b);
  }
  inline void assert_remaining_at_least(word_t n) {
    // TODO: make this runtime error, not assertion
    G_ASSERT(m_limit - m_ptr > n);
  }
  Str read_string(word_t size) {
    assert_remaining_at_least(size);
    Str result;
    result.reserve(size);
    for (auto i = 0; i < size; ++i) {
      result += (char)read_byte();
    }
    return result;
  }
  // TODO: Sanity check for overflow?
  template <typename T>
  T read_var() {
    T result = 0;
    u8_t b = read_byte();
    while (b & 0x80) {
      result *= 0x7f;
      result += (T)(b & 0x7f);
    }
    return result + (T)b;
  }

  inline void read_bytes(u8_t *dst, word_t sz) {
    std::copy(m_ptr, m_ptr+sz, dst);
    m_ptr += sz;
  }
};

class LoaderState {
public:
  Vector<Str> m_atoms;
  UniquePtr<u8_t> m_code;

  void load_atom_table(Reader &r);
  void load_fun_table(Reader &r) {
    auto sz = r.read_var<word_t>();
    G_ASSERT(sz == 0);
  }
  void load_export_table(Reader &r) {
    auto sz = r.read_var<word_t>();
    G_ASSERT(sz == 0);
  }
  void load_code(Reader &r);
  void load_literal_table(Reader &r);
};

void CodeServer::load_module(Term name_atom, const u8_t *bytes, word_t size) {
  G_ASSERT(name_atom.is_atom() || name_atom.is_nil());
  Reader r(bytes, size);
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

void LoaderState::load_atom_table(Reader &r)
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

void LoaderState::load_code(Reader &r) {
  auto sz = r.read_var<word_t>();
  r.assert_remaining_at_least(sz);

  auto dptr = mem::alloc_bytes(sz).get_result(); // TODO: feeling lucky
  m_code.reset(dptr);
  r.read_bytes(dptr, sz);
}

void LoaderState::load_literal_table(Reader &r)
{
  auto bytes_sz = r.read_var<word_t>();
  r.assert_remaining_at_least(bytes_sz);

}

} // ns gluon
