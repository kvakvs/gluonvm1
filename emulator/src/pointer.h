#pragma once

//
// Pointer knowledge for the current OS/CPU
// Such as: high and low bits which are safe to use for tags
// Tagging and untagging
// Address ranges (global ranges and check heaps in VM)
//
#include "defs.h"

namespace gluon {

enum class PointerTag {
  None,
  Continuation
};

#ifdef __linux__
class Linux64PointerKnowledge {
public:
  // how many bits are safe to cut and throw away for user-space pointers
  constexpr static Word high_bits = 1;
  constexpr static Word high_mask = (~0ULL) << (word_bitsize - high_bits);
  // how many bits will always be zero due to pointer alignment
  constexpr static Word low_bits = (word_bitsize == 64? 3 : 2);
  constexpr static Word low_mask = (~0ULL) >> (word_bitsize - high_bits);
  constexpr static Word mask = high_mask | low_mask;

  Linux64PointerKnowledge() = delete;

  template <typename T>
  static bool is_userspace_pointer(T* p) {
    return ((Word)p & ~mask) == 0;
  }

  template <typename T>
  static T* untag(T* p) {
    return (T*)((Word)p & ~mask);
  }

  template <typename T>
  static PointerTag high_tag(T* p) {
    return (PointerTag)(((Word)p & high_mask) >> high_bits);
  }

  template <typename T>
  static T* set_high_tag(T* p, PointerTag tag) {
    return (T*)(((Word)p & high_mask) | ((Word)tag << high_bits));
  }

  template <typename T>
  static PointerTag low_tag(T* p) {
    return (PointerTag)((Word)p & low_mask);
  }

  template <typename T>
  static T* set_low_tag(T* p, PointerTag tag) {
    return (T*)(((Word)p & low_mask) | (Word)tag);
  }
};
#endif // linux

#ifdef __linux__
using PointerKnowledge = Linux64PointerKnowledge;
#endif

} // ns gluon
