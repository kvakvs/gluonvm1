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
#else
#   error "Define platform bit width detection code"
#endif

// If you enable this, bignums have to be implemented throughout loader and vm
#define FEATURE_BIGNUM    0
// If you enable this, floats have to be implemented throughout loader and vm
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

  template <typename A, typename B>
  using Pair = std::pair<A, B>;

  template <typename A>
  using List = std::list<A>;

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
  class label_index_t {
  public:
    word_t value;
    label_index_t(): value(0) {}
    explicit label_index_t(word_t x): value(x) {}
  };

//  // Index in code table, wrapped to create a distinct compile-time type
//  typedef struct code_offset_t {
//    word_t value;
//    static inline code_offset_t wrap(word_t x) {
//      code_offset_t co;
//      co.value = x;
//      return co;
//    }
//  } code_offset_t;

//  class Module;
//  typedef struct {
//    Module *module;
//    code_offset_t offset;
//  } code_ptr_t;

  static const word_t VM_MAX_REGS = 16; // max arity of fun which can be called
  static const word_t VM_MAX_FP_REGS = 2;

} // ns gluon

// Branch prediction helper macros, use when something is going to happen much
// more or much less often
#define G_LIKELY(x)       __builtin_expect((x),1)
#define G_UNLIKELY(x)     __builtin_expect((x),0)

#define G_NORETURN __attribute__((noreturn))

#define G_FAIL(MSG) ::fprintf(stderr, "FAIL: %s (%s:%d)\n", MSG, __FILE__, __LINE__); ::abort();

// TODO: debug macro goes here
#if G_DEBUG
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

#   define G_ASSERT(X)
#   define G_ASSERT_MSG(X, MSG)
#   define G_TODO(X)
#   define G_LOG dummy_printf
#   define G_IF_NODEBUG(X) X
#endif

// TODO: borrow hot/cold table or build my own
#define G_ATTR_HOT __attribute((__hot__))
#define G_ATTR_COLD __attribute((__cold__))
