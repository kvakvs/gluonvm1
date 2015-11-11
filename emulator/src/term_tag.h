#pragma once
// TO BE INCLUDED FROM TERM.H ONLY

#include "pointer.h"
#include "wrap.h"

namespace gluon {
namespace term_tag {

template <typename T>
static Word compress_pointer(T* p) {
  return reinterpret_cast<Word>(p);
}
template <typename T>
static T* expand_pointer(Word p) {
  return reinterpret_cast<T*>(p);
}

// Primary and immediate tagging:
//
// xxxx00 cons
// xxxx01 tuple
// xxxx10 boxed (subtag)
// 000011 atom
// 100011 catch index
// 001011 short pid
// 010011 short oid
// 011011 (unused)
// 101011 (unused) overloaded as reg X
// 110011 (unused) overloaded as slot Y
// 111011 nil,noval,rip
// xxx111 small integer

enum class PrimaryTag {
  Cons = 0,
  Tuple = 1,
  Boxed = 2,
  Immediate1 = 3,
};

//
// Group of operations on level 0 tag (stores tag in bits 0,1)
//
const Word Primary_tag_size = 2;

template <PrimaryTag TagType>
struct Level0Tag {
  const static Word Mask = 0x3;
  constexpr static bool check(Word x) { return (x & Mask) == (Word)TagType; }
  template <typename T>
  static Word create_from_ptr(T* p) {
    return compress_pointer(p) | (Word)TagType;
  }
  template <typename T>
  static T* expand_ptr(Word x) {
    return expand_pointer<T>(x & ~Mask);
  }
  constexpr static Word create(Word v) {
    return (v << Primary_tag_size) | (Word)TagType;
  }
  constexpr static Word value(Word t) { return (t >> Primary_tag_size); }
};

typedef Level0Tag<PrimaryTag::Cons> Cons;
typedef Level0Tag<PrimaryTag::Tuple> Tuple;
typedef Level0Tag<PrimaryTag::Boxed> Boxed;
typedef Level0Tag<PrimaryTag::Immediate1> Immed;

//
// Templatized tag/untag/check functions for level 1 tags (bits 2,3 when
// bits 0,1 are equal to IMMED1)
//
enum class Imm1Tag {
  Atom = 0,  //(0 << PRIMARY_SIZE) | IMMED1,
  SmallInt = 1,
  ShortPid = 2,
  ShortPort = 4,
  FpRegister = 6,
  Catch = 8,
  XRegister = 10,
  YRegister = 12,
  Special = 14,  // includes nil,noval,rip
};

//
// Group of operations on level 1 tag (stores tag in bits 2,3,4,5 while bits
// 0,1 contain 0x03)
//
template <Imm1Tag TagType>
struct Level1Tag {
  const static Word Mask = 0x3F;  // 6 least bits = 1
  // level 1 tag bits shifted left | level 0 Immediate1 tag
  const static Word Tag_incl_L0L1 =
      ((Word)TagType << Primary_tag_size) | (Word)PrimaryTag::Immediate1;
  const static Word L1_tag_bits = 6;

  constexpr static bool check(Word x) { return (x & Mask) == Tag_incl_L0L1; }
  constexpr static Word create(Word v) {
    return (v << L1_tag_bits) | Tag_incl_L0L1;
  }
  constexpr static Word value(Word t) { return t >> L1_tag_bits; }
};

typedef Level1Tag<Imm1Tag::Atom> Atom;
typedef Level1Tag<Imm1Tag::ShortPid> ShortPid;
typedef Level1Tag<Imm1Tag::ShortPort> ShortPort;
typedef Level1Tag<Imm1Tag::Catch> Catch;
typedef Level1Tag<Imm1Tag::FpRegister> FpRegister;
typedef Level1Tag<Imm1Tag::XRegister> XRegister;
typedef Level1Tag<Imm1Tag::YRegister> YRegister;
typedef Level1Tag<Imm1Tag::Special> Special;  // includes nil,noval,rip

struct Smallint {
  const static Word Tag = 0x1;
  const static Word Mask = 0x7;

  // level 1 bits shifted left with IMMED1 level 0 tag together
  const static Word Tag_incl_L0L1 =
      (Tag << Primary_tag_size) | (Word)PrimaryTag::Immediate1;
  const static Word L1_tag_bits = 3;

  constexpr static bool check(Word x) { return (x & Mask) == Tag_incl_L0L1; }
  constexpr static Word create(SWord v) {
    return (Word)(v << L1_tag_bits) | Tag_incl_L0L1;
  }
  constexpr static Word create_u(Word v) {
    return (Word)(v << L1_tag_bits) | Tag_incl_L0L1;
  }
  constexpr static SWord value(Word t) { return ((SWord)t) >> L1_tag_bits; }
  constexpr static Word value_u(Word t) { return t >> L1_tag_bits; }
};

//
// Boxed subtags
//
const Word boxed_subtag_bits = 4;
const Word boxed_subtag_mask = 0x0F;
const Word boxed_max_subtag_val =
    (1UL << (gluon::word_bitsize - boxed_subtag_bits)) - 1;

inline Word boxed_subtag(Word* p) {
  return p[0] & boxed_subtag_mask;
}

enum class BoxedSubtag {
  PositiveBignum = 0,
  NegativeBignum = 1,
  Float = 2,
  Map = 3,
  FunObject = 6,
  Export = 7,
  Pid = 8,
  Port = 9,
  Ref = 10,
  DestroyedSomething = 11,
  ProcBinary = 12,
  HeapBinary = 13,
  MatchContext = 14,
  SubBinary = 15,
};

// Takes least 4 bits of subtag
static constexpr BoxedSubtag get_subtag(Word x) {
  return (BoxedSubtag)(x & boxed_subtag_mask);
}
// Removes least 4 bits of subtag returning what's left
static constexpr Word get_subtag_value(Word x) {
  return x >> boxed_subtag_bits;
}
// Takes m_val from Term, converts to pointer and reads subtag (least 4 bits)
static inline BoxedSubtag unbox_and_get_subtag(Word x) {
  return (BoxedSubtag)(Boxed::expand_ptr<Word>(x)[0] & boxed_subtag_mask);
}

template <BoxedSubtag Subtag>
struct TaggedBox {
  // Takes a term value, converts to pointer, checks if it was boxed, then
  // follows the pointer and checks that first word is tagged with SUBTAG
  static bool unbox_and_check(Word x) {
    return Boxed::check(x) && unbox_and_get_subtag(x) == Subtag;
  }
  static constexpr bool check_subtag(Word x) { return get_subtag(x) == Subtag; }
  template <typename T>
  static Word create_from_ptr(T* p) {
    return Boxed::create_from_ptr<T>(p);
  }
  template <typename T>
  static T* expand_ptr(Word x) {
    return Boxed::expand_ptr<T>(x);
  }
  static constexpr Word create_subtag(Word x) {
    return (x << boxed_subtag_bits) | (Word)Subtag;
  }
};

typedef TaggedBox<BoxedSubtag::PositiveBignum> BoxedPosBignum;
typedef TaggedBox<BoxedSubtag::NegativeBignum> BoxedNegBignum;
typedef TaggedBox<BoxedSubtag::Float> BoxedFloat;
typedef TaggedBox<BoxedSubtag::Map> BoxedMap;
typedef TaggedBox<BoxedSubtag::FunObject> BoxedFun;
typedef TaggedBox<BoxedSubtag::Export> BoxedExport;
typedef TaggedBox<BoxedSubtag::Pid> BoxedPid;
typedef TaggedBox<BoxedSubtag::Port> BoxedPort;
typedef TaggedBox<BoxedSubtag::Ref> BoxedRef;
typedef TaggedBox<BoxedSubtag::DestroyedSomething> BoxedRIP;
typedef TaggedBox<BoxedSubtag::ProcBinary> BoxedProcBin;
typedef TaggedBox<BoxedSubtag::HeapBinary> BoxedHeapBin;
typedef TaggedBox<BoxedSubtag::MatchContext> BoxedMatchCtx;
typedef TaggedBox<BoxedSubtag::SubBinary> BoxedSubBin;

}  // ns term_tag


//
// Continuation Pointer (CP) is a term-looking value tagged as Boxed in its
// low bits, and having PointerHTag::Continuation in its high bits.
// It is used as return address and also marks stack frame
//

class ContinuationPointer {
  Word value_;

public:
  explicit ContinuationPointer(Word x): value_(x) {}
  ContinuationPointer(): value_(0) {}

  Word value() const { return value_; }

  // Is a valid continuation
  bool check() const {
    return (value_ != 0)
        && PointerKnowledge::high_tag(value_) == PointerHTag::Continuation
      #ifdef G_DEBUG
        && PointerKnowledge::low_tag(value_) == PointerLTag::Boxed
      #endif
        ;
  }

  // Set highest bit to mark CP pushed on stack
  static ContinuationPointer make_cp(CodePointer x) {
    // just in case, hope word x is not already tagged
    G_ASSERT(false == ContinuationPointer((Word)x.value()).check());
    G_ASSERT(PointerKnowledge::is_userspace_pointer(x.value()));
    return ContinuationPointer(
          PointerKnowledge::set_tags(x.value(),
                                     PointerHTag::Continuation,
                                     PointerLTag::Boxed)
          );
  }

  // Check and clear highest bit to mark CP pushed on stack
  CodePointer untag() const {
    G_ASSERT(check() == true);
    auto result = PointerKnowledge::untag<Word*>(value_);
    G_ASSERT(PointerKnowledge::is_userspace_pointer(result));
    return CodePointer(result);
  }
};


} // ns gluon
