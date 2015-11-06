#pragma once
// TO BE INCLUDED FROM TERM.H ONLY

//
// Define layouts of boxes for boxed terms, tuple and cons
//
namespace layout {
// Cons layout has no place for bit tag
struct Cons {
  static const Word box_word_size = 2;

  template <typename Cell>
  static Cell& head(Cell* box) {
    static_assert(sizeof(Cell) == sizeof(Word), "bad cell size");
    return box[0];
  }

  template <typename Cell>
  static Cell& tail(Cell* box) {
    static_assert(sizeof(Cell) == sizeof(Word), "bad cell size");
    return box[1];
  }

  template <typename Cell>
  static Cell& element(Cell* box, Word i) {
    static_assert(sizeof(Cell) == sizeof(Word), "bad cell size");
    return box[i];
  }
};

// Tuple layout has no place for bit tag.
// First goes arity, then elements
struct Tuple {
  static const Word box_extra_words = 1;

  static Word box_size(Word Arity) { return Arity + box_extra_words; }

  template <typename Cell>
  static Cell& arity(Cell* box) {
    static_assert(sizeof(Cell) == sizeof(Word), "bad cell size");
    return box[0];
  }

  template <typename Cell>
  static Cell& element(Cell* box, Word i) {
    static_assert(sizeof(Cell) == sizeof(Word), "bad cell size");
    return box[i + box_extra_words];
  }
};

// Process-heap (small) and heap (large) binary layout
struct Binary {
  // both types of binary have size in first (subtag) word
  static Word get_byte_size(Word* p) {
    return term_tag::get_subtag_value(p[0]);
  }
};

// Box structure
// Word { size, tag_bits: 4 }; u8_t data[size]
struct ProcBin {
  static const Word box_extra_words = 1;

  static Word box_size(Word bytes);

  static void set_byte_size(Word* box, Word bytes) {
    box[0] = term_tag::BoxedProcBin::create_subtag(bytes);
  }

  template <typename Cell>
  static Cell* data(Cell* box) {
    static_assert(sizeof(Cell) == sizeof(Word), "bad cell size");
    return box + 1;
  }
};
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wzero-length-array"
// Heapbin is stored on far heap with refcount field
// Box only contains size and pointer to far heap with heapbin
class HeapbinBox {
 private:
  Word m_size;  // contains both size and boxed tag
  Word m_refcount;
  Word m_data[0];

 public:
  void set_byte_size(Word bytes) {
    m_size = term_tag::BoxedHeapBin::create_subtag(bytes);
  }

  template <typename Cell>
  Cell* data() {
    // static_assert(sizeof(Cell) == sizeof(Word), "bad cell size");
    return (Cell*)&m_data;
  }

  Word refcount() const { return m_refcount; }
  void set_refcount(Word r) { m_refcount = r; }
};
#pragma clang diagnostic pop

struct HeapBin {
  static const Word farheap_extra_words = sizeof(HeapbinBox) / sizeof(Word);

  static Word box_size(Word bytes);
};

}  // ns layout
