#include "vm_ctx.h"

namespace gluon {
namespace impl {

CheckBifError VMRuntimeContext::check_bif_error(Process *p) {
  if (p->is_not_failed()) {
    return CheckBifError::None;
  }
  p->handle_error();
  return CheckBifError::ErrorOccured;
}

}  // ns impl
}  // ns gluon
