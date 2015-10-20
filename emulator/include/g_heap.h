#pragma once

#include "g_defs.h"
#include "g_error.h"
#include "gsys_mem.h"

//#include <memory>
//#include <vector>

namespace gluon {

class Term;

template <typename T>
static constexpr word_t calculate_storage_size() {
  return ((sizeof(T) + sizeof(word_t) - 1) / sizeof(word_t)) * sizeof(word_t);
}
static constexpr word_t calculate_word_size(word_t bytes) {
  return ((bytes + sizeof(word_t) - 1) / sizeof(word_t)) * sizeof(word_t);
}

namespace mem {

  //
  // Takes memory blocks directly from system_memory (whatever that is) without
  // any additional tricks, segmenting, grouping by size, free-lists and so on
  //
  class SystemMemoryAllocator {
  public:
    template <typename T>
    T *allocate() { return (T*)system_memory::allocate(sizeof(T)); }

    template <typename T>
    T *allocate(size_t n) {
      mem::Blk blk = system_memory::allocate(n * sizeof(T));
      return (T*)blk.mem();
    }

    template <typename T, typename... Args>
    inline T *alloc_object(Args&&... args) { // NOTE: calls ctor
      word_t *bytes = allocate<word_t>(calculate_storage_size<T>());
      return new(bytes)T(std::forward<Args>(args)...);
    }

    template <typename T>
    void deallocate(T *mem) {
      mem::Blk mem1(mem, 0);
      system_memory::deallocate(mem1);
    }
  };

} // ns mem

namespace vm {

// VM heap is abstract interface which gets memory from underlying system.
class Heap: public mem::SystemMemoryAllocator {
};

} // ns vm

namespace proc {

static constexpr word_t DEFAULT_PROC_HEAP_WORDS = 100000;
static constexpr word_t DEFAULT_PROC_STACK_WORDS = 5000;
// Allocated heap segments sequentially grow from DEFAULT_PROC_HEAP_WORDS (100)
// up to 2^HEAP_SEGMENT_GROWTH_MAX (100*2^8=100kb on 32bit or 200kb on 64bit)
// All new blocks after 8th will be capped at this size.
static constexpr word_t HEAP_SEGMENT_GROWTH_MAX = 8;

class Heap;

// Heap node for process heap. We begin with small enough heap and grow
// by allocating double that every time when we run out of memory in last
// node. Fields of Node take first words of the heap then follows free memory.
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wzero-length-array"
class Node {
public:
  static constexpr word_t FIELDS_WORD_SIZE = 3; // how many words this class takes
  static_assert(DEFAULT_PROC_STACK_WORDS < DEFAULT_PROC_HEAP_WORDS - FIELDS_WORD_SIZE,
                "default stack does not fit default heap size");

  Node *next = nullptr;
  word_t *start;  // this points at first free space and grows
  word_t *limit;  // marks end of node
  word_t heap_start[0]; // marks end of headers and beginning of heap

  static Node *create(word_t sz_words);

  word_t get_avail() const {
    G_ASSERT(limit >= start);
    return (word_t)(limit - start);
  }
  // Allocated memory is not tagged in any way except regular term bitfields
  word_t *allocate_words(word_t n) {
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
/*
class SegmentedHeap {
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
  SegmentedHeap();

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
}; // class SegmentedHeap
*/

class Heap: public mem::SystemMemoryAllocator {
public:
  Stack m_stack;
};


// Takes all terms between 'start' and 'end', and copies them to 'dstheap', new
// resulting terms are placed in array 'dst' which should be large enough.
bool copy_terms(Heap *dstheap, const Term *start, const Term *end, Term *dst);
// Copies one term 't' to 'dstheap' returns new clone term located in new heap
Term copy_one_term(Heap *dstheap, Term t);

} // ns proc
} // ns gluon
