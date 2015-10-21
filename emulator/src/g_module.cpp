#include "g_module.h"
#include "g_reader.h"
#include "g_vm.h"
//#include "g_heap.h"

namespace gluon {

word_t *Module::resolve_label(label_index_t label)
{
  if (label.value >= labels_.size()) {
    throw err::beam_load_error("label index too big");
  }
  return labels_[label.value];
}

void Module::set_exports(Module::exports_t &e) {
  exports_ = std::move(e);

  // Replace known BIFs in exports with their BIF pointer and flag them as such
  auto exps = exports_.all();
  for_each_keyvalue(exps, [this](const fun_arity_t &fa, const export_t &exp) {
                    void *bif_ptr = vm_->find_bif(mfarity_t(name_, fa));
                    if (bif_ptr) {
                      exports_[fa] = export_t(bif_ptr);
                    }
                  });
}

fun_arity_t Module::find_fun_arity(word_t *ptr) const
{
  return fun_index_.find(ptr);
}

#if FEATURE_CODE_RANGES
code::Range Module::get_code_range() {
  return code::Range(code_.data(), (&code_.back())+1);
}
#endif

} // ns gluon
