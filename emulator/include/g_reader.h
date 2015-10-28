#pragma once

#include "g_defs.h"
#include "struct/g_array.h"

namespace gluon {
namespace tool {

class Reader {
  const Uint8 *ptr_;
  const ArrayView<const Uint8> view_;

public:
//  Reader(const u8_t *ptr, Word size): m_ptr(ptr), m_limit(ptr+size) {
//  }
  Reader(ArrayView<const Uint8> vw)
    : ptr_(vw.data()), view_(vw) {
  }
  // Clones reader with m_ptr value, sets new size, to read smaller limited
  // parts of input data
  Reader clone(Word new_size) {
    G_ASSERT(ptr_ + new_size <= view_.limit());
    Reader new_r(ArrayView<const Uint8>(ptr_, new_size));
    return new_r;
  }

  inline Uint8 peek_byte() {
    return *ptr_;
  }
  inline Uint8 read_byte() {
    // TODO: make this runtime error, not assertion
    G_ASSERT(ptr_ < view_.limit());
    // FIXME: am i really not having 1 byte overlap here?
    return *ptr_++;
  }

  // Advance by 1 byte, assert its value equal to 'value'
  void assert_byte(Uint8 value) {
    G_ASSERT(value == read_byte());
  }
  inline void assert_remaining_at_least(Word n) const {
    // TODO: make this runtime error, not assertion
    G_ASSERT((view_.limit() - ptr_) >= (SWord)n);
  }
  inline Word get_remaining_count() const {
    G_ASSERT(view_.limit() >= ptr_);
    return (Word)(view_.limit() - ptr_);
  }
  inline bool is_end() const {
    return view_.limit() <= ptr_;
  }
  const Uint8 *get_ptr() const {
    return ptr_;
  }

  Str read_string(Word size) {
    assert_remaining_at_least(size);
    Str result;
    result.reserve(size);
    for (Word i = 0; i < size; ++i) {
      result += (char)read_byte();
    }
    return result;
  }

  void read_bytes(Uint8 *dst, Word sz) {
    std::copy(ptr_, ptr_+sz, dst);
    ptr_ += sz;
  }

  Word read_big_u16() {
    Word result = ((Word)ptr_[0] << 8)  | (Word)ptr_[1];
    ptr_ += 2;
    return result;
  }
  Word read_big_u32() {
    Word result = ((Word)ptr_[0] << 24) | ((Word)ptr_[1] << 16)
                  | ((Word)ptr_[2] << 8)  | (Word)ptr_[3];
    ptr_ += 4;
    return result;
  }
  SWord read_big_s(Word bytes) {
    SWord result = read_byte();
    if (result & 128) {
      // set all bytes above first to 0xFF
      result = (SWord)((~0xFFul) | (Word)result);
    }
    for (Word i = 1; i < bytes; i++) {
      result <<= 8;
      result += read_byte();
    }
    return result;
  }
  inline void advance(Word x) {
    assert_remaining_at_least(x);
    ptr_ += x;
  }
  template <Word ALIGN>
  inline void advance_align(Word x) {
    assert_remaining_at_least(x);
    ptr_ += ALIGN * ((x + ALIGN - 1) / ALIGN);
  }
};

} // ns tool
} // ns gluon
