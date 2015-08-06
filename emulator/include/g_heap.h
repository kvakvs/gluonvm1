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

  static u8_t *alloc_bytes(Heap *, word_t bytes) {
    return new u8_t[bytes];
  }

  template <typename T>
  static inline T *alloc(Heap *h, word_t count) {
    // NOTE: does not call constructors
    return reinterpret_cast<T *>(alloc_bytes(h, count * sizeof(T)));
  }

  template <typename T, typename... Args>
  static inline T *alloc_object(Heap *h, Args&&... args) { // NOTE: calls ctor
    u8_t *bytes = alloc_bytes(h, sizeof(T));
    return new(bytes)T(std::forward<Args>(args)...);
  }

  static void free_bytes(Heap *, u8_t *p) {
    delete p;
    return;
  }

  template <typename T>
  static void free(Heap *h, T *p) { // NOTE: does not call dtor
    return free_bytes(h, reinterpret_cast<u8_t *>(p));
  }
};

static const word_t DEFAULT_PROC_HEAP_WORDS = 100;

// Heap node for growing process heap. We begin with small enough heap and grow
// every time when we run out of memory in last node.
class PHNode {
public:
  PHNode *m_next;
  word_t *m_start; // this points at first free space and grows
  word_t *m_limit; // marks end of node

  inline word_t get_avail() const {
    G_ASSERT(m_limit < m_start);
    return (word_t)(m_limit - m_start);
  }
};

class ProcessHeap {
  //
  // Stack
  //
  PHNode    *m_stack_node = nullptr;
  word_t    *m_s_top;     // stack tip, grows down from heap end
  word_t    *m_s_bottom;  // stack bottom, delimits stack growth

  //
  // Heap
  //
  PHNode    *m_current = &m_first;

public:
  // Created by Process class
  ProcessHeap() {
    m_heap.resize(DEFAULT_PROC_HEAP_WORDS);
    m_sp = &m_heap.back();
    m_htop = &m_heap.front();
  }

  template <typename T>
  static constexpr word_t calculate_storage_size() {
    return ((sizeof(T) + sizeof(word_t) - 1) / sizeof(word_t)) * sizeof(word_t);
  }
  static constexpr word_t calculate_word_size(word_t bytes) {
    return ((bytes + sizeof(word_t) - 1) / sizeof(word_t)) * sizeof(word_t);
  }

  // Grows htop and gives requested memory
  word_t *h_alloc(word_t);

  // TODO: Mark memory so that GC will know its size
  inline word_t *h_alloc_bytes(word_t bytes) {
    return h_alloc(calculate_word_size(bytes));
  }

  // TODO: Mark memory so that GC will know its size
  template <typename T, typename... Args>
  inline T *h_alloc_object(Args&&... args) { // NOTE: calls ctor
    word_t *bytes = h_alloc(calculate_storage_size<T>());
    return new(bytes)T(std::forward<Args>(args)...);
  }
};

} // ns gluon
