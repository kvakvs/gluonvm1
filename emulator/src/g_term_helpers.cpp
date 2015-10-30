#include "g_term_helpers.h"

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

}  // ns term
}  // ns gluon
