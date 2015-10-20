#include "gsys_mem.h"
//#include "g_error.h"

namespace gluon {
namespace mem {

  Blk CppStdlibMemory::allocate(size_t bytes) {
    return Blk(new unsigned char[bytes], bytes);
  }

  void CppStdlibMemory::deallocate(Blk &p) {
    delete (unsigned char *)p.mem();
  }

} // ns mem
} // ns gluon
