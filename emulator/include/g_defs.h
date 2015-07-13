#pragma once

#include <string>
#include <map>

#define G_HAVE_EXCEPTIONS 0

namespace gluon {
  using Str = std::basic_string<char>;
  using UStr = std::basic_string<uint8_t>;

  template <typename K, typename V>
  using Map = std::map<K, V>;

  // Hardware abstractions
  namespace hw {
    using Word = long unsigned int; // size of machine word
    using SWord = long;
  } // ns hw

  using word_t = hw::Word;
  using sword_t = hw::SWord;
  using u8_t = unsigned char;
  using i8_t = signed char;
  using u16_t = unsigned char;
  using i16_t = signed char;
  using u32_t = unsigned int;
  using i32_t = signed int;
  using u64_t = unsigned long long;
  using i64_t = signed long long;

} // ns gluon

// Branch prediction helper macros, use when something is going to happen much
// more or much less often
#define G_LIKELY(x)       __builtin_expect((x),1)
#define G_UNLIKELY(x)     __builtin_expect((x),0)

#define G_NORETURN __attribute__((noreturn))

// TODO: debug macro goes here
#define G_FAIL(MSG) ::fprintf(stderr, "%s", "ASSERTION: " MSG "\n"); ::abort();
#define G_ASSERT(X) if (!(X)) { G_FAIL(#X); }
#define G_ASSERT_MSG(X, MSG) if (!(X)) { G_FAIL(MSG); }
#define G_TODO(what) {                    \
  ::fprintf(stderr, "TODO: %s\n", what);  \
  G_ASSERT(what == nullptr)               \
  }
