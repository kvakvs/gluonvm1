#include "test.h"
#include "g_code_index.h"
#include <fructose/fructose.h>

#pragma clang diagnostic ignored "-Wweak-vtables"
using namespace gluon;

namespace gluontest {
struct range_test_t: public fructose::test_base<range_test_t>
{

  void test_range_basics(const std::string& test_name) {
    code::Index<int> i;
    i.add(code::Range((word_t*)100, (word_t*)200), 1);
    fructose_assert(i.find((word_t *)150) == 1);
    fructose_assert(i.find((word_t *)100) == 1);
    fructose_assert(i.find((word_t *)199) == 1);
    fructose_assert(i.find((word_t *)200) == 0); // upper not inclusive

    i.add(code::Range((word_t*)250, (word_t*)300), 3);
    i.add(code::Range((word_t*)1000, (word_t*)2000), 2);

    fructose_assert(i.find((word_t *)1000) == 2);
    fructose_assert(i.find((word_t *)1500) == 2);
    fructose_assert(i.find((word_t *)1999) == 2);
    fructose_assert(i.find((word_t *)2000) == 0); // upper not inclusive

    fructose_assert(i.find((word_t *)250) == 3);
    fructose_assert(i.find((word_t *)275) == 3);
    fructose_assert(i.find((word_t *)299) == 3);
    fructose_assert(i.find((word_t *)300) == 0); // upper not inclusive
  }

}; // struct

void gluon_test_ranges(int argc, const char *argv[]) {
  range_test_t tests;
  tests.add_test("codeindex.ranges", &range_test_t::test_range_basics);
  tests.run(argc, const_cast<char **>(argv));
}

} // ns gluontest
