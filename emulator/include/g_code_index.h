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
  word_t  *start;
  word_t  *end;   // one word after code end

  Range(): start(nullptr), end(nullptr) {
  }
  Range(word_t *s, word_t *e): start(s), end(e) {
  }

  inline bool contains(word_t *p) const {
    // we only compare single pointer and store it in start, 'end' should be null
    return p >= start && p < end;
  }
  inline bool operator <(const Range &other) const {
    // Assume ranges don't overlap so we can only compare starts
    return start < other.start;
  }
};

//template <typename T>
//inline bool custom_compare(const std::pair<Range, T> &a,
//                           const std::pair<Range, T> &b) {
//  return a.first < b.first;
//}

template <typename T>
class Index {
private:
  Dict<Range, T> ranges_;

public:
  Index() {}

  // Register new loaded code as range [start,end)
  void add(const Range &r, T value) {
    ranges_[r] = value;
  }

  // Find code location in tree of ranges
  T find(word_t *x) const
  {
    // I cannot into range search, something with lower_bound/upper_bound which
    // compares ranges using operator < and that is too hard
    // TODO: fix this
    auto allranges = ranges_.all();
    while (allranges.have()) {
      if (allranges.key().contains(x)) {
        return allranges.value();
      }
      allranges.advance();
    }
    return T();
//    auto find = std::make_pair(Range(x, nullptr), T());
//    auto i = std::lower_bound(m_ranges.begin(), m_ranges.end(), find,
//                              custom_compare<T>);
//    if (i->first.contains(x)) {
//      return i->second;
//    }
//    return T();
  }
};
#endif

#if G_TEST
void range_test(int argc, const char *argv[]);
#endif // TEST

} // ns code
} // ns gluon
