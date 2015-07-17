#pragma once

#include <string>
#include <map>
#include <list>
#include <vector>
#include <memory>

#define G_DEBUG           1
#define G_TEST            0
#define G_HAVE_EXCEPTIONS 0

#if __BYTE_ORDER == __LITTLE_ENDIAN
#   define G_BIGENDIAN      0
#else
#   define G_BIGENDIAN      1
#endif

#if __GNUC__
#   if __x86_64__ || __ppc64__
#       define G_HARDWARE_BITS 64
#   else
#       define G_HARDWARE_BITS 32
#   endif
#endif

#define FEATURE_BIGNUM    0
#define FEATURE_FLOAT     0
#define FEATURE_MAPS      0
#define FEATURE_BINARIES  0
#define FEATURE_ERL_DIST  0 /*Distribution features*/

namespace gluon {
  constexpr unsigned int get_hardware_bits() {
    return (8*sizeof(void*));
  }

  // An STL-compatible string of char and unsigned char
  using Str = std::basic_string<char>;
  //using UStr = std::basic_string<u8_t>;

  // An STL-compatible dictionary, keys should have compare operator <
  template <typename K, typename V>
  using Map = std::map<K, V>;

  // An STL-compatible double linked list
//  template <typename V>
//  using List = std::list<V>;

  // An STL-compatible resizable array (actually can transition to fixed arrays)
  template <typename V>
  using Vector = std::vector<V>;

  // Self-deleting RAII-style pointer holder
  template <typename V>
  using UniquePtr = std::unique_ptr<V>;

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

  using float_t = float;
  using f32_t = float;
  using f64_t = double;

  // Used to shun debug printfs in release
  inline void dummy_printf(const char *, ...) {}

  // Index in label table, wrapped to create a distinct compile-time type
  typedef struct label_index_t {
    word_t value;
    static inline label_index_t wrap(word_t x) {
      label_index_t co;
      co.value = x;
      return co;
    }
  } label_index_t;

  // Index in code table, wrapped to create a distinct compile-time type
  typedef struct code_offset_t {
    word_t value;
    static inline code_offset_t wrap(word_t x) {
      code_offset_t co;
      co.value = x;
      return co;
    }
  } code_offset_t;

} // ns gluon

// Branch prediction helper macros, use when something is going to happen much
// more or much less often
#define G_LIKELY(x)       __builtin_expect((x),1)
#define G_UNLIKELY(x)     __builtin_expect((x),0)

#define G_NORETURN __attribute__((noreturn))

// TODO: debug macro goes here
#if G_DEBUG
#   define G_FAIL(MSG) ::fprintf(stderr, "FAIL: %s\n", MSG); ::abort();
#   define G_ASSERT(X) if (!(X)) { G_FAIL(#X); }
#   define G_ASSERT_MSG(X, MSG) if (!(X)) { G_FAIL(MSG); }
#   define G_TODO(what) {                    \
      ::fprintf(stderr, "TODO: %s\n", what);  \
      G_ASSERT(what == nullptr)               \
      }
    // Famous io:format/2 skill on Linkedin!
#   define G_LOG ::printf
#   define G_IF_NODEBUG(X)

#else // no G_DEBUG

#   define G_FAIL(MSG)
#   define G_ASSERT(X)
#   define G_ASSERT_MSG(X, MSG)
#   define G_TODO(X)
#   define G_LOG dummy_printf
#   define G_IF_NODEBUG(X) X
#endif
