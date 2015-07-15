#pragma once

#include "g_defs.h"

namespace gluon {
namespace tool {

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
  inline void assert_remaining_at_least(word_t n) const {
    // TODO: make this runtime error, not assertion
    G_ASSERT((m_limit - m_ptr) > n);
  }
  inline word_t get_remaining_count() const {
    return (m_limit - m_ptr);
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
//    // read no more than 5 (9 on 64-bit) 7-bit chunks for sanity
//    int limit = G_HARDWARE_BITS / 7 + 1;
    u8_t b = read_byte();
    while (b & 0x80) {
      result <<= 7;
      result += (T)b & 0x7f;
      b = read_byte();

//      limit--;
//      G_ASSERT(limit >= 0);
    }
    result <<= 7;
    return result + (T)b;
  }

  void read_bytes(u8_t *dst, word_t sz) {
    std::copy(m_ptr, m_ptr+sz, dst);
    m_ptr += sz;
  }

  word_t read_bigendian_i16() {
    word_t result = (m_ptr[0] << 8)  | m_ptr[1];
    m_ptr += 2;
    return result;
  }

  word_t read_bigendian_i32() {
    word_t result = (m_ptr[0] << 24) | (m_ptr[1] << 16)
                  | (m_ptr[2] << 8)  | m_ptr[3];
    m_ptr += 4;
    return result;
  }
};

} // ns tool
} // ns gluon
