#pragma once

#if G_TEST
namespace gluontest {

void run_tests(int argc, const char *argv[]);

void gluon_test_terms(int argc, const char *argv[]);
void gluon_test_processes(int argc, const char *argv[]);
void gluon_test_ranges(int argc, const char *argv[]);

} // ns gluontest
#endif
