#pragma once

#include <string>
#include <map>
#include <list>
#include <vector>
#include <memory>
#include <queue>
//#include <stack>

// set features for use in code in this file
#include "g_FEATURE.h"

// Default value (set in CMakeLists.txt, do not edit here)
#ifndef G_DEBUG
#   define G_DEBUG 1
#endif

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
#else
#   error "Define platform bit width detection code"
#endif

#define FMT_HEX "%zx"
#define FMT_0xHEX "0x" FMT_HEX
#define FMT_SWORD "%zi"
#define FMT_UWORD "%zu"

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

  // Self-deleting refcounted copyable pointer
  template <typename V>
  using RcPtr = std::shared_ptr<V>;

  template <typename A, typename B>
  using Pair = std::pair<A, B>;

  template <typename A>
  using List = std::list<A>;

  // Single-ended queue with push to end/pop front operations
  template <typename A>
  using Queue = std::queue<A>;

//  // Stack with single end and index operations
//  template <typename A>
//  using Stack = std::stack<A>;

  // Hardware abstractions
  namespace hw {
    using Word = long unsigned int; // size of machine word
    using SWord = long;
  } // ns hw

  using word_t = hw::Word;
  using sword_t = hw::SWord;
  constexpr inline word_t word_size(word_t x) {
    return (x + sizeof(word_t) - 1) / sizeof(word_t);
  }

  using u8_t = unsigned char;
  using i8_t = signed char;
  using u16_t = unsigned char;
  using s16_t = signed char;
  using u32_t = unsigned int;
  using s32_t = signed int;
  using u64_t = unsigned long long;
  using s64_t = signed long long;

#if FEATURE_FLOAT
  using float_t = float;
  using f32_t = float;
  using f64_t = double;
#endif

  // Used to shun debug printfs in release
  inline void dummy_printf(const char *, ...) {}

  // Index in label table, wrapped to create a distinct compile-time type
  class label_index_t {
  public:
    word_t value;
    label_index_t(): value(0) {}
    explicit label_index_t(word_t x): value(x) {}
  };

  namespace vm {
    // How many reds will a process be allowed to run before next proc wakes up
    // Adjust this for slow devices. 2000 is used for regular modern hardware.
    static const word_t SLICE_REDUCTIONS = 1000;

    static const word_t MAX_FUN_ARITY = 16;
    static const word_t MAX_REGS = 64; // (max arity of fun + captured terms)
    static const word_t MAX_STACK = 128; // is not enforced anywhere yet
    static const word_t MAX_FP_REGS = 2;
  } // vm

#if FEATURE_LINE_NUMBERS
  namespace line {
    constexpr bool is_valid_loc(word_t File, word_t Line) {
        return (File < 255 && Line < ((1 << 24) - 1));
    }
    constexpr word_t make_location(word_t File, word_t Line) {
      return (File << 24) | Line;
    }
    constexpr word_t get_loc_file(word_t Loc) {
      return Loc >> 24;
    }
    constexpr word_t get_loc_line(word_t Loc) {
      return Loc & ((1 << 24)-1);
    }

    const static word_t INVALID_LOC = make_location(0, 0);
  } // ns line
#endif


} // ns gluon

// Branch prediction helper macros, use when something is going to happen much
// more or much less often
#define G_LIKELY(x)       __builtin_expect((x),1)
#define G_UNLIKELY(x)     __builtin_expect((x),0)

#define G_NORETURN __attribute__((noreturn))

#define G_FAIL(MSG) ::fprintf(stderr, "FAIL: %s (%s:%d)\n", MSG, __FILE__, __LINE__); ::abort();

// TODO: debug macro goes here
#if G_DEBUG
#   define G_HINT_INLINE
#   define G_ASSERT(X) if (!(X)) { G_FAIL(#X); }
#   define G_ASSERT_MSG(X, MSG) if (!(X)) { G_FAIL(MSG); }
#   define G_TODO(what) { \
      ::fprintf(stderr, "TODO: %s (%s:%d)\n", what, __FILE__, __LINE__);  \
      G_ASSERT(what == nullptr) \
      }
    // Famous io:format/2 skill on Linkedin!
#   define G_LOG ::printf
#   define G_IF_NODEBUG(X)

#else // no G_DEBUG

#   define G_HINT_INLINE inline
#   define G_ASSERT(X)
#   define G_ASSERT_MSG(X, MSG)
#   define G_TODO(X)
#   define G_LOG dummy_printf
#   define G_IF_NODEBUG(X) X
#endif

// TODO: borrow hot/cold table or build my own
#define G_ATTR_HOT __attribute((__hot__))
#define G_ATTR_COLD __attribute((__cold__))
