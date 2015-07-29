#pragma once

#include "g_defs.h"

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

  inline bool operator ==(const Range &p) const {
    // we only compare single pointer and store it in start, 'end' should be null
    G_ASSERT(p.end == nullptr);
    return p.start >= start && p.start < end;
  }
  inline bool operator <(const Range &other) const {
    // Assume ranges don't overlap so we can only compare starts
    return start < other.start;
  }
};

template <typename T>
class Index {
  Map<Range, T> m_ranges;

public:
  Index() {}

  // Register new loaded code as range [start,end)
  void add(const Range &r, T value) {
    m_ranges[r] = value;
  }

  // Find code location in tree of ranges
  Module *find(word_t *x) const
  {
    auto iter = m_ranges.find(Range(x, nullptr));
    if (iter == m_ranges.end()) {
      return nullptr;
    }
    return iter->second;
  }
};
#endif

} // ns code
} // ns gluon
