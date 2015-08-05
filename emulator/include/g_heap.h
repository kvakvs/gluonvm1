#pragma once

#include "g_defs.h"
#include <memory>
//#include <vector>

namespace gluon {

class Term;


template<typename T>
struct ZoneAlloc
{
  typedef T value_type;

  ZoneAlloc() = default;

  template<typename U> ZoneAlloc(const ZoneAlloc<U> &) {}
  ZoneAlloc(const ZoneAlloc &) {}
  ZoneAlloc & operator=(const ZoneAlloc &) { return *this; }
  ZoneAlloc(ZoneAlloc &&) = default;
  ZoneAlloc & operator=(ZoneAlloc &&) = default;

  typedef std::true_type propagate_on_container_copy_assignment;
  typedef std::true_type propagate_on_container_move_assignment;
  typedef std::true_type propagate_on_container_swap;

  bool operator==(const ZoneAlloc & other) const {
    return this == &other;
  }
  bool operator!=(const ZoneAlloc & other) const {
    return !(*this == other);
  }

  T * allocate(size_t num_to_allocate) {
    if (num_to_allocate != 1) {
      return static_cast<T *>(::operator new(sizeof(T) * num_to_allocate));
    }
    else if (m_avail.empty()) {
      // first allocate 8, then double whenever
      // we run out of memory
      size_t to_allocate = 8 << m_mem.size();
      m_avail.reserve(to_allocate);
      std::unique_ptr<value_holder[]> allocated(new value_holder[to_allocate]);
      value_holder * first_new = allocated.get();
      m_mem.emplace_back(std::move(allocated));
      size_t to_return = to_allocate - 1;
      for (size_t i = 0; i < to_return; ++i) {
          m_avail.push_back(std::addressof(first_new[i].value));
      }
      return std::addressof(first_new[to_return].value);
    } else {
      T * result = m_avail.back();
      m_avail.pop_back();
      return result;
    }
  }
  void deallocate(T * ptr, size_t num_to_free) {
    if (num_to_free == 1) {
        m_avail.push_back(ptr);
    } else {
        ::operator delete(ptr);
    }
  }

  // boilerplate that shouldn't be needed, except
  // libstdc++ doesn't use allocator_traits yet
  template<typename U>
  struct rebind {
      typedef ZoneAlloc<U> other;
  };
  typedef T * pointer;
  typedef const T * const_pointer;
  typedef T & reference;
  typedef const T & const_reference;
  template<typename U, typename... Args>
  void construct(U * object, Args &&... args) {
    new (object) U(std::forward<Args>(args)...);
  }
  template<typename U, typename... Args>
  void construct(const U * object, Args &&... args) = delete;
  template<typename U>
  void destroy(U * object) {
    object->~U();
  }

private:
  union value_holder {
    value_holder() {}
    ~value_holder() {}
    T value;
  };

  Vector<std::unique_ptr<value_holder[]>> m_mem;
  Vector<T *> m_avail;
};

class Heap {
public:
  Heap() = delete;

  // h = nullptr will allocate on global C++ heap for debugging
  static u8_t *alloc_bytes(Heap *h, word_t bytes);

  template <typename T>
  static inline T *alloc(Heap *h, word_t count) {
    // NOTE: does not call constructors
    return reinterpret_cast<T *>(alloc_bytes(h, count * sizeof(T)));
  }

  template <typename T, typename... Args>
  static inline T *alloc_object(Heap *h, Args&&... args) {
    // NOTE: will call constructor
    u8_t *bytes = alloc_bytes(h, sizeof(T));
    return new(bytes)T(std::forward<Args>(args)...);
  }

  static void free_bytes(Heap *h, u8_t *p);

  template <typename T>
  static void free(Heap *h, T *p) {
    // NOTE: does not call dtor
    return free_bytes(h, reinterpret_cast<u8_t *>(p));
  }
  // Marks nested terms as unused (assuming no references to them)
  static void free_terms(Heap *h, Term *terms, word_t /*count*/) {
    // TODO: marking
    free(h, terms);
  }
};

} // ns gluon
