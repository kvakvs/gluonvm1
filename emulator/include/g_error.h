#pragma once

#include <functional>
#include "g_defs.h"
#include "platf/gsys_stdlib.h"

#define G_DEBUG_THROW_E if(e) { \
  gluon::Std::fmt("debug throw: %s\n", e); \
  gluon::Std::abort(); \
  }

namespace gluon {

//
// Wraps pair of error C string and result. Can either flag an error
// (a C string) or carries result value of type T
//
template <typename T>
class Result {
  const char *m_what;
  T           m_result;
public:
  Result(const char *e, T result)
    : m_what(e), m_result(result) {
    G_DEBUG_THROW_E
  }
  inline bool is_error() const { return m_what != nullptr; }
  inline const char *get_error() const { return m_what; }
  inline bool is_success() const { return m_what == nullptr; }
  inline void clear() {
    m_what = nullptr;
    m_result = T();
  }

  // Feeling lucky? call something().get_result() directly
  inline T get_result() const {
    G_ASSERT(is_success());
    return m_result;
  }

  // Repacks error into another type of Result for easier returning
  template <typename U>
  inline Result<U> rewrap_error() {
    G_ASSERT(is_error());
    return Result<U>(m_what, U());
  }
};
template <typename T>
inline static Result<T> error(const char *e) {
  return Result<T>(e, T());
}
template <typename T>
inline static Result<T> success(T result) {
  return Result<T>(nullptr, result);
}

//
// Wraps error C string for a void function. Can only flag error but carries
// no result value. Supports formatting of error message, for this it attempts
// to allocate memory and print into it using standard library.
//
class MaybeError {
private:
  const char *m_error;
  //bool        m_owned_memory;
public:
  MaybeError(): m_error(nullptr)/*, m_owned_memory(false)*/ {}
  MaybeError(const char *e): m_error(e) /*, m_owned_memory(false)*/ {
    G_DEBUG_THROW_E
  }
//  MaybeError(const char *e, bool own): m_error(e), m_owned_memory(own) {
//    G_DEBUG_THROW_E
//  }
  ~MaybeError() {
//    if (m_owned_memory) {
//      delete m_error;
//    }
  }

  inline void clear() {
    m_error = nullptr;
//    m_owned_memory = false;
  }
  // Move ctor and move assignment
  MaybeError(MaybeError &&other) {
    m_error = other.m_error;
//    m_owned_memory = other.m_owned_memory;

    other.m_error = nullptr;
//    other.m_owned_memory = false;
  }
  MaybeError &operator =(MaybeError &&other) {
    if (this != &other) {
      m_error = other.m_error;
//      m_owned_memory = other.m_owned_memory;

      other.m_error = nullptr;
//      other.m_owned_memory = false;
    }
    return *this;
  }

  inline bool is_error() const { return m_error != nullptr; }
  inline const char *get_error() const { return m_error; }
  inline bool is_success() const { return m_error == nullptr; }

  // Repacks error into MaybeResult for easier returning
  template <typename U>
  inline Result<U> rewrap_error() {
    G_ASSERT(is_success());
    return Result<U>(m_error, U());
  }
};

//template <class Err=MaybeError, typename... Args>
//static Err err_fmt(const char *_fmt, Args&&... args) {
//  char *err = new char[256]; // TODO: scale this or something
//  ::sprintf(err, _fmt, std::forward<Args>(args)...);
//  return Err(err, true);
//}

static inline MaybeError success() { return MaybeError(); }

//
// Errors are handled by two classes MaybeError and Result<T>, both identically
// can have error set as const char *. Result<T> can carry value of T as result
// (then error is guaranteed to be nullptr). The following macros allow to check
// error state and forward it out of the current function.  Rewrap macro allows
// to convert MaybeError into Result<T>. UNLIKELY and LIKELY macros help
// branch prediction.
//

// Returns const char * reason of the error (auto converted to MaybeError)
#define G_RETURN_IF_ERROR(res) \
  if (res.is_error()) { return res.get_error(); }
// Wrapped in unlikely, because we're happy to expect success
#define G_RETURN_IF_ERROR_UNLIKELY(res) \
  if (G_UNLIKELY(res.is_error())) { return res.get_error(); }
// Same but error is likely to happen
#define G_RETURN_IF_ERROR_LIKELY(res) \
  if (G_LIKELY(res.is_error())) { return res.get_error(); }

// Returns reason of MaybeResult rewrapped into another MaybeResult
#define G_RETURN_REWRAP_IF_ERROR(res,T) \
  if (res.is_error()) { return res.rewrap_error<T>(); }
// Wrapped in unlikely, because we're happy to expect success
#define G_RETURN_REWRAP_IF_ERROR_UNLIKELY(res,T) \
  if (G_UNLIKELY(res.is_error())) { return res.rewrap_error<T>(); }
// Same but error is likely to happen
#define G_RETURN_REWRAP_IF_ERROR_LIKELY(res,T) \
  if (G_LIKELY(res.is_error())) { return res.rewrap_error<T>(); }

} // ns gluon
