#pragma once

#include <list>
#include <memory>
#include <queue>
#include <set>
//#include "gsl/array_view.h"
#include <cstdint>

// set features for use in code in this file
#include "FEATURE.h"

// Default value (set in CMakeLists.txt, do not edit here)
#ifndef G_DEBUG
#define G_DEBUG 1
#endif

#if __BYTE_ORDER == __LITTLE_ENDIAN
#define G_BIGENDIAN 0
#else
#define G_BIGENDIAN 1
#endif

#if __GNUC__
#if __x86_64__ || __ppc64__
#define G_HARDWARE_BITS 64
#else
#define G_HARDWARE_BITS 32
#endif
#else
#error "Define platform bit width detection code"
#endif

namespace gluon {

#if G_DEBUG
constexpr bool debug_mode = true;
#else
constexpr bool debug_mode = false;
#endif

#define FMT_HEX "%zx"
#define FMT_0xHEX "0x" FMT_HEX
#define FMT_SWORD "%zi"
#define FMT_UWORD "%zu"

#define DECL_EXCEPTION(NAME)                       \
  class NAME : public std::runtime_error {         \
   public:                                         \
    NAME(const char* e) : std::runtime_error(e) {} \
    virtual const char* what() const noexcept;     \
  };
#define IMPL_EXCEPTION(NAME) \
  const char* NAME::what() const noexcept { return std::runtime_error::what(); }
#define DECL_IMPL_EXCEPTION(NAME) DECL_EXCEPTION(NAME) IMPL_EXCEPTION(NAME)

namespace err {
DECL_EXCEPTION(FeatureMissing)
DECL_EXCEPTION(TODO)
DECL_EXCEPTION(BeamLoad)
DECL_EXCEPTION(Scheduler)
DECL_EXCEPTION(CodeServer)
DECL_EXCEPTION(Process)
}  // ns err

constexpr unsigned int word_bitsize = G_HARDWARE_BITS;

// template <typename T>
// using array_view = gsl::array_view<T>;

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

template <typename A>
using SingleList = std::list<A>;

// Single-ended queue with push to end/pop front operations
template <typename A>
using Queue = std::queue<A>;

template <typename A>
using Set = std::set<A>;

using Word = std::size_t;
using SWord = std::ptrdiff_t;

constexpr Word word_size(Word x) {
  return (x + sizeof(Word) - 1) / sizeof(Word);
}

using Uint8 = std::uint8_t;
using Int8 = std::int8_t;
using Uint16 = std::uint16_t;
using Int16 = std::int16_t;
using Uint32 = std::uint32_t;
using Int32 = std::int32_t;
using Uint64 = std::uint64_t;
using Int64 = std::int64_t;

using Float32 = float;
using Float64 = double;
using Float = Float32;

// Used to shun debug printfs in release
inline void dummy_printf(const char*, ...) {}

namespace erts {
// How many reds will a process be allowed to run before next proc wakes up
// Adjust this for slow devices. 2000 is used for regular modern hardware.
constexpr Word reductions_per_slice = 250;

constexpr Word max_fun_arity = 16;
constexpr Word max_regs = 64;    // (max arity of fun + captured terms)
constexpr Word max_stack = 128;  // is not enforced anywhere yet
constexpr Word max_fp_regs = 2;
}  // vm

#if FEATURE_LINE_NUMBERS
namespace line {
constexpr bool is_valid_loc(Word File, Word Line) {
  return (File < 255 && Line < ((1 << 24) - 1));
}
constexpr Word make_location(Word File, Word Line) {
  return (File << 24) | Line;
}
constexpr Word get_loc_file(Word Loc) {
  return Loc >> 24;
}
constexpr Word get_loc_line(Word Loc) {
  return Loc & ((1 << 24) - 1);
}

const static Word invalid_location = make_location(0, 0);
}  // ns line
#endif

namespace Std {
// Used by G_ASSERT macro
void assert_fail(const char* what, const char* file, int line);
}  // ns Std

}  // ns gluon

// Branch prediction helper macros, use when something is going to happen much
// more or much less often
#define G_LIKELY(x) __builtin_expect((x), 1)
#define G_UNLIKELY(x) __builtin_expect((x), 0)

#define G_NORETURN __attribute__((noreturn))

//#define G_FAIL(MSG) ::fprintf(stderr, "FAIL: %s (%s:%d)\n", MSG, __FILE__,
//__LINE__); ::abort();

// TODO: debug macro goes here
#define G_ASSERT(X)                           \
  if (debug_mode && !(X)) {                   \
    Std::assert_fail(#X, __FILE__, __LINE__); \
  }
#define G_TODO(what)                                                    \
  if (debug_mode) {                                                     \
    ::fprintf(stderr, cYellow cBold "TODO:" cRst " %s (%s:%d)\n", what, \
              __FILE__, __LINE__);                                      \
    throw gluon::err::TODO(what);                                       \
  }
#if G_DEBUG
// Famous io:format/2 skill on Linkedin!
#define G_LOG gluon::Std::fmt
#define G_IF_NODEBUG(X)

#else  // no G_DEBUG

//#define G_ASSERT(X)
//#define G_TODO(X)
#define G_LOG dummy_printf
#define G_IF_NODEBUG(X) X
#endif

// TODO: borrow hot/cold table or build my own
#define G_ATTR_HOT __attribute((__hot__))
#define G_ATTR_COLD __attribute((__cold__))

//
// Colors!
//
// FOREGROUND
#define cRst "\x1B[0m"
#define cBlack "\x1B[30m"
#define cRed "\x1B[31m"
#define cGreen "\x1B[32m"
#define cYellow "\x1B[33m"
#define cBlue "\x1B[34m"
#define cMagenta "\x1B[35m"
#define cCyan "\x1B[36m"
#define cWhite "\x1B[37m"

#define bBlack "\x1B[40m"
#define bRed "\x1B[41m"
#define bGreen "\x1B[42m"
#define bYellow "\x1B[43m"
#define bBlue "\x1B[44m"
#define bMagenta "\x1B[45m"
#define bCyan "\x1B[46m"
#define bWhite "\x1B[47m"

#define cBold "\x1B[1m"
#define cUnderline "\x1B[4m"
#define cItalic "\x1B[3m"
#define cInverse "\x1B[7m"
#define cStrike "\x1B[9m"
#define noBold "\x1B[21m"
#define noUnderline "\x1B[24m"
#define noItalic "\x1B[23m"
#define noInverse "\x1B[27m"
#define noStrike "\x1B[29m"

#define tRed(x) cRed x cRst
#define tGreen(x) cGreen x cRst
#define tYellow(x) cYellow x cRst
#define tBlue(x) cBlue x cRst
#define tMagenta(x) cMagenta x cRst
#define tCyan(x) cCyan x cRst
#define tWhite(x) cWhite x cRst
#define tBold(x) cBold x cRst
#define tUnderline(x) cUnderline x cRst
