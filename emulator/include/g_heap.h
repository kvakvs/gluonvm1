#pragma once

#include "g_defs.h"
#include "g_error.h"
#include "platf/gsys_mem.h"
#include "g_stack.h"

//#include <memory>
//#include <vector>

namespace gluon {

class VM;
class Term;

template <typename T>
static constexpr Word calculate_storage_size() {
  return ((sizeof(T) + sizeof(Word) - 1) / sizeof(Word)) * sizeof(Word);
}
static constexpr Word calculate_word_size(Word bytes) {
  return ((bytes + sizeof(Word) - 1) / sizeof(Word)) * sizeof(Word);
}

namespace mem {

//
// Takes memory blocks directly from system_memory (whatever that is) without
// any additional tricks, segmenting, grouping by size, free-lists and so on
//
class SystemMemoryAllocator {
 public:
  template <typename T>
  T* allocate() {
    return SysMemory::allocate<T>();
  }

  template <typename T>
  T* allocate(size_t n) {
    mem::Blk<T> blk = SysMemory::allocate<T>(n);
    return blk.mem();
  }

  template <typename T, typename... Args>
  T* alloc_object(Args&&... args) {  // NOTE: calls ctor
    Word* bytes = allocate<Word>(calculate_storage_size<T>());
    return new (bytes) T(std::forward<Args>(args)...);
  }

  template <typename T>
  void deallocate_one(T* mem) {
    mem::Blk<T> mem1(mem, 1);
    SysMemory::deallocate<T>(mem1);
  }

  template <typename T>
  void deallocate_many(T* mem, size_t sz) {
    mem::Blk<T> mem1(mem, sz);
    SysMemory::deallocate<T>(mem1);
  }
};

}  // ns mem

namespace erts {

// VM heap is abstract interface which gets memory from underlying system.
class Heap : public mem::SystemMemoryAllocator {};

}  // ns vm

namespace proc {

static constexpr Word DEFAULT_PROC_HEAP_WORDS = 100000;
static constexpr Word DEFAULT_PROC_STACK_WORDS = 5000;
// Allocated heap segments sequentially grow from DEFAULT_PROC_HEAP_WORDS (100)
// up to 2^HEAP_SEGMENT_GROWTH_MAX (100*2^8=100kb on 32bit or 200kb on 64bit)
// All new blocks after 8th will be capped at this size.
static constexpr Word HEAP_SEGMENT_GROWTH_MAX = 8;

class Heap;

// Heap node for process heap. We begin with small enough heap and grow
// by allocating double that every time when we run out of memory in last
// node. Fields of Node take first words of the heap then follows free memory.
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wzero-length-array"
class Node {
 public:
  static constexpr Word FIELDS_WORD_SIZE =
      3;  // how many words this class takes
  static_assert(DEFAULT_PROC_STACK_WORDS <
                    DEFAULT_PROC_HEAP_WORDS - FIELDS_WORD_SIZE,
                "default stack does not fit default heap size");

  Node* next = nullptr;
  Word* start;         // this points at first free space and grows
  Word* limit;         // marks end of node
  Word heap_start[0];  // marks end of headers and beginning of heap

  static Node* create(Word sz_words);

  Word get_avail() const {
    G_ASSERT(limit >= start);
    return (Word)(limit - start);
  }
  // Allocated memory is not tagged in any way except regular term bitfields
  Word* allocate_words(Word n) {
    auto result = start;
    start += n;
    return result;
  }
};
#pragma clang diagnostic pop


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
  Word m_node_count = 1;
  Node  *m_current;
  Node  *m_root;

public:
  Stack m_stack;

public:
  // Allocates DEFAULT_PROC_HEAP_WORDS and writes Node header there, uses it as
  // initial heap node
  SegmentedHeap();

  // Grows htop and gives requested memory
  Word *h_alloc(Word);

  // TODO: Mark memory so that GC will know its size
  Word *h_alloc_bytes(Word bytes) {
    return h_alloc(calculate_word_size(bytes));
  }

  // TODO: Mark memory so that GC will know its size
  template <typename T, typename... Args>
  T *h_alloc_object(Args&&... args) { // NOTE: calls ctor
    Word *bytes = h_alloc(calculate_storage_size<T>());
    return new(bytes)T(std::forward<Args>(args)...);
  }
}; // class SegmentedHeap
*/

template <class A>
class Heap_ : public A {
  constexpr static Word STK_SZ = 1024;
  Word stack_data_[STK_SZ];

 public:
  Stack stack_;

  Heap_() : A(), stack_(&stack_data_[0], &stack_data_[STK_SZ]) {}
};

class Heap : public Heap_<mem::SystemMemoryAllocator> {};

// Takes all terms between 'start' and 'end', and copies them to 'dstheap', new
// resulting terms are placed in array 'dst' which should be large enough.
bool copy_terms(VM& vm,
                Heap* dstheap,
                const Term* start,
                const Term* end,
                Term* dst);
// Copies one term 't' to 'dstheap' returns new clone term located in new heap
Term copy_one_term(VM& vm, Heap* dstheap, Term t);

}  // ns proc
}  // ns gluon
