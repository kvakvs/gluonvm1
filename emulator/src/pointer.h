#pragma once
#include "defs.h"

#include "platf/gsys_stdlib.h" // for debug print

namespace gluon {

enum class PointerHTag {
  None = 0,
  Continuation = 1
};
enum class PointerLTag {
  None = 0,
  Boxed = 2 // same as in term_tag.h for primary tag to look like a boxed
};

//
// Pointer knowledge for the current OS/CPU
// Such as: high and low bits which are safe to use for tags
// Tagging and untagging
// Address ranges (global ranges and check heaps in VM)
//
// Interface:
// is_userspace_pointer(p) checks if pointer looks like a legal address in
//    program's memory
// untag, set_high/low_tag, high/low_tag - clears, sets, reads tags added
//    to free bits of pointer. Low tags match Term tags to have a tagged
//    pointer that looks like a boxed value or something else. High tags mark
//    type of pointer.

#ifdef __linux__
template <Word total_bits, Word high_bits>
class LinuxPointerKnowledge {
public:
  static_assert(total_bits == sizeof(void *) * 8,
                "bad total_bits in PointerKnowledge");

  // how many bits are safe to cut and throw away for user-space pointers
  constexpr static Word high_pos = total_bits - high_bits;
  constexpr static Word high_mask = (Word)(~0ULL) << high_pos;
  // how many bits will always be zero due to pointer alignment
  constexpr static Word low_bits = (total_bits == 64? 3 : 2);
  constexpr static Word low_mask = (Word)(~0ULL) >> (total_bits - low_bits);
  constexpr static Word mask = high_mask | low_mask;

  LinuxPointerKnowledge() = delete;

  static void assert() {
    if (debug_mode) {
      G_ASSERT(is_userspace_pointer((void *)0x7fff'f7f8'd070ULL));
    }
  }

  template <typename T>
  static bool has_no_tags(T* p) {
    return ((Word)p & mask) == 0;
  }

  template <typename T>
  static bool is_userspace_pointer(T* p) {
    // Must be in range for userspace for this platform
    // Must be aligned (no extra 1 in low bits)
    return ((Word)p <= 0x7fff'ffff'ffffULL)
        && has_no_tags(p);
  }

  template <typename T>
  static T untag(Word p) {
    return (T)(p & ~mask);
  }

  template <typename T>
  static Word set_tags(T* p, PointerHTag htag, PointerLTag ltag) {
    Word stripped = (Word)p & ~mask;
    Word res = stripped | ((Word)htag << high_pos) | (Word)ltag;
    //Std::fmt("cont set_tags 0x%zx\n", res);
    return res;
  }

  static PointerHTag high_tag(Word p) {
    return (PointerHTag)((Word)p >> (total_bits - high_bits));
  }

  template <typename T>
  static Word set_high_tag(T* p, PointerHTag tag) {
    return ((Word)p & high_mask) | ((Word)tag << high_bits);
  }

  static PointerLTag low_tag(Word p) {
    return (PointerLTag)((Word)p & low_mask);
  }

  template <typename T>
  static Word set_low_tag(T* p, PointerLTag tag) {
    return ((Word)p & low_mask) | (Word)tag;
  }
};
#endif // linux

#ifdef __linux__
// Currently 48 bits of address are used on 64-bit linux which allows to
// address something like 256Tb of RAM. Change this when they switch to using
// 56 or 64bit.
using PointerKnowledge = LinuxPointerKnowledge<64, 16>;
#endif

} // ns gluon
