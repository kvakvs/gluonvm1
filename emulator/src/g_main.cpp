#include "g_vm.h"

int main(int argc, const char *argv[]) {

  // VM has all static members, do not instantiate
  gluon::VM::init();

  return 0;
}
