#include "term_helpers.h"

namespace gluon {
namespace term {

Term build_string(proc::Heap* heap, const Str& s) {
  return build_list(heap, s.begin(), s.end());
}

Term build_string(proc::Heap* h, const char* cstr) {
  const char* cstr_end;
  for (cstr_end = cstr; *cstr_end; ++cstr_end) {
  }
  return build_list(h, cstr, cstr_end);
}

Term make_tuple(proc::Heap *heap, const std::initializer_list<Term> &values) {
  TupleBuilder tb(heap, values.size());
  for (auto v: values) {
    tb.add(v);
  }
  return tb.make_tuple();
}

}  // ns term
}  // ns gluon
