#pragma once

#include "g_defs.h"
#include "struct/g_dict.h"

#include <algorithm>

namespace gluon {

class Module;

namespace code {

#if FEATURE_CODE_RANGES
class Range {
 public:
  Word* start;
  Word* end;  // one word after code end

  Range() : start(nullptr), end(nullptr) {}
  Range(Word* s, Word* e) : start(s), end(e) {}

  bool contains(Word* p) const {
    // we only compare single pointer and store it in start, 'end' should be
    // null
    return p >= start && p < end;
  }
  bool operator<(const Range& other) const {
    // Assume ranges don't overlap so we can only compare starts
    return start < other.start;
  }
};

template <typename T>
class Index {
 private:
  Dict<Range, T> ranges_;

 public:
  Index() {}

  // Register new loaded code as range [start,end)
  void add(const Range& r, T value) { ranges_.insert(r, value); }

  // Find code location in tree of ranges
  bool find(Word* x, T& out) const {
    // I cannot into range search, something with lower_bound/upper_bound which
    // compares ranges using operator < and that is too hard
    // TODO: fix this
    auto rng = ranges_.all();
    while (rng.have()) {
      auto kv = rng.current();
      if (kv->first.contains(x)) {
        out = kv->second;
        return true;
      }
      rng.advance();
    }
    return false;
  }
};
#endif

#if G_TEST
void range_test(int argc, const char* argv[]);
#endif  // TEST

}  // ns code
}  // ns gluon
