#pragma once

#include "g_defs.h"
#include <memory>
//#include <vector>

namespace gluon {

class Term;

#if 0
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
#endif //0


template <typename T>
static constexpr word_t calculate_storage_size() {
  return ((sizeof(T) + sizeof(word_t) - 1) / sizeof(word_t)) * sizeof(word_t);
}
static constexpr word_t calculate_word_size(word_t bytes) {
  return ((bytes + sizeof(word_t) - 1) / sizeof(word_t)) * sizeof(word_t);
}

namespace vm {

// VM heap is abstract interface which gets memory from underlying system.
// Simplification: using standard new and delete
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

} // ns vm
namespace proc {

static const word_t DEFAULT_PROC_HEAP_WORDS = 100;
static const word_t DEFAULT_PROC_STACK_WORDS = 50;
// Allocated heap segments sequentially grow from DEFAULT_PROC_HEAP_WORDS (100)
// up to 2^HEAP_SEGMENT_GROWTH_MAX (100*2^8=100kb on 32bit or 200kb on 64bit)
// All new blocks after 8th will be capped at this size.
static const word_t HEAP_SEGMENT_GROWTH_MAX = 8;

class Heap;

// Heap node for process heap. We begin with small enough heap and grow
// by allocating double that every time when we run out of memory in last
// node. Fields of Node take first words of the heap then follows free memory.
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wzero-length-array"
class Node {
public:
  static const word_t FIELDS_WORD_SIZE = 3; // how many words this class takes
  static_assert(DEFAULT_PROC_STACK_WORDS < DEFAULT_PROC_HEAP_WORDS - FIELDS_WORD_SIZE,
                "default stack does not fit default heap size");

  Node *next = nullptr;
  word_t *start;  // this points at first free space and grows
  word_t *limit;  // marks end of node
  word_t heap_start[0]; // marks end of headers and beginning of heap

  static Node *create(word_t sz_words);

  inline word_t get_avail() const {
    G_ASSERT(limit >= start);
    return (word_t)(limit - start);
  }
  // Allocated memory is not tagged in any way except regular term bitfields
  inline word_t *allocate_words(word_t n) {
    auto result = start;
    start += n;
    return result;
  }
};
#pragma clang diagnostic pop

//
// Stack
//
class Stack {
  Node      *m_node = nullptr;  // where stack is
  word_t    *m_end;             // stack underflow mark
  word_t    *m_top;             // stack tip, grows down from heap end
  word_t    *m_bottom;          // stack bottom, delimits stack growth

public:
  // Lowers 'limit' by 'size' words, puts stack there
  void put_stack(Node *h_node, word_t size);

  void set_y(word_t index, word_t value) {
    G_ASSERT(get_used() >= index + 1);
    m_top[index+1] = value;
  }
  word_t get_y(word_t index) const {
    G_ASSERT(get_used() >= index + 1);
    return m_top[index+1];
  }
  void push(word_t x) {
    G_ASSERT(get_avail() > 0);
    m_top--;
    *m_top = x;
  }
  word_t pop() {
    G_ASSERT(get_used() > 0);
    auto result = *m_top;
    m_top++;
    return result;
  }
  void push_n_nils(word_t n);
  void drop_n(word_t n) {
    G_ASSERT(get_used() >= n);
    m_top += n;
  }
  word_t get_avail() const {
    G_ASSERT(m_top >= m_bottom);
    return (word_t)(m_top - m_bottom);
  }
  word_t get_used() const {
    G_ASSERT(m_top <= m_end);
    return (word_t)(m_end - m_top);
  }
};

//
// Segmented heap which grows by allocating double the last segment size
// and never shrinks (until process dies). Also contains stack which may migrate
// out of its home node.
//
class Heap {
  //
  // Heap
  //
  // track count of nodes allocated, each new is double of previous
  word_t m_node_count = 1;
  Node  *m_current;
  Node  *m_root;

public:
  Stack m_stack;

public:
  // Allocates DEFAULT_PROC_HEAP_WORDS and writes Node header there, uses it as
  // initial heap node
  Heap();

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
}; // class Heap

} // ns proc
} // ns gluon
