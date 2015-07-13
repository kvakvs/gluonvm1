#include "g_vm.h"

using vm = gluon::VM;

int main(int argc, const char *argv[]) {

  vm::init();
  vm::load_module("../test/g_test1.S.gleam");

  return 0;
}
