#pragma once

#include "g_defs.h"
#include "struct/g_array.h"

namespace gluon {
namespace tool {

class Reader {
  const u8_t *ptr_;
  const array_view<const u8_t> view_;

public:
//  Reader(const u8_t *ptr, word_t size): m_ptr(ptr), m_limit(ptr+size) {
//  }
  Reader(array_view<const u8_t> vw)
    : ptr_(vw.data()), view_(vw) {
  }
  // Clones reader with m_ptr value, sets new size, to read smaller limited
  // parts of input data
  Reader clone(word_t new_size) {
    G_ASSERT(ptr_ + new_size <= view_.limit());
    Reader new_r(array_view<const u8_t>(ptr_, new_size));
    return new_r;
  }
  Reader clone() {
    Reader new_r(array_view<const u8_t>(ptr_, (word_t)(view_.limit() - ptr_)));
    return new_r;
  }

  inline u8_t peek_byte() {
    return *ptr_;
  }
  inline u8_t read_byte() {
    // TODO: make this runtime error, not assertion
    G_ASSERT(ptr_ < view_.limit());
    // FIXME: am i really not having 1 byte overlap here?
    return *ptr_++;
  }

  // Advance by 1 byte, assert its value equal to 'value'
  void assert_byte(u8_t value) {
    G_ASSERT(value == read_byte());
  }
  inline void assert_remaining_at_least(word_t n) const {
    // TODO: make this runtime error, not assertion
    G_ASSERT((view_.limit() - ptr_) >= (sword_t)n);
  }
  inline word_t get_remaining_count() const {
    G_ASSERT(view_.limit() >= ptr_);
    return (word_t)(view_.limit() - ptr_);
  }
  inline bool is_end() const {
    return view_.limit() <= ptr_;
  }
  const u8_t *get_ptr() const {
    return ptr_;
  }

  Str read_string(word_t size) {
    assert_remaining_at_least(size);
    Str result;
    result.reserve(size);
    for (word_t i = 0; i < size; ++i) {
      result += (char)read_byte();
    }
    return result;
  }

  void read_bytes(u8_t *dst, word_t sz) {
    std::copy(ptr_, ptr_+sz, dst);
    ptr_ += sz;
  }

  word_t read_big_u16() {
    word_t result = ((word_t)ptr_[0] << 8)  | (word_t)ptr_[1];
    ptr_ += 2;
    return result;
  }
  word_t read_big_u32() {
    word_t result = ((word_t)ptr_[0] << 24) | ((word_t)ptr_[1] << 16)
                  | ((word_t)ptr_[2] << 8)  | (word_t)ptr_[3];
    ptr_ += 4;
    return result;
  }
  sword_t read_big_s(word_t bytes) {
    sword_t result = read_byte();
    if (result & 128) {
      // set all bytes above first to 0xFF
      result = (sword_t)((~0xFFul) | (word_t)result);
    }
    for (word_t i = 1; i < bytes; i++) {
      result <<= 8;
      result += read_byte();
    }
    return result;
  }
  inline void advance(word_t x) {
    assert_remaining_at_least(x);
    ptr_ += x;
  }
  template <word_t ALIGN>
  inline void advance_align(word_t x) {
    assert_remaining_at_least(x);
    ptr_ += ALIGN * ((x + ALIGN - 1) / ALIGN);
  }
};

} // ns tool
} // ns gluon
