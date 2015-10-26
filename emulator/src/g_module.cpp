#include "g_module.h"
#include "g_reader.h"
#include "g_vm.h"
#include "g_functional.h"

namespace gluon {

word_t *Module::resolve_label(LabelIndex label)
{
  if (label.value() >= labels_.size()) {
    throw err::beam_load_error("label index too big");
  }
  return labels_[label.value()];
}

void Module::set_exports(Module::exports_t &e) {
  exports_ = std::move(e);

  // Replace known BIFs in exports with their BIF pointer and flag them as such
  auto exps = exports_.all();
  for_each(exps, [this](auto fa_exp) {
                    void *bif_ptr = vm_->find_bif(mfarity_t(name_, fa_exp->first));
                    if (bif_ptr) {
                      exports_[fa_exp->first] = export_t(bif_ptr);
                    }
                  });
}

#if FEATURE_CODE_RANGES
fun_arity_t Module::find_fun_arity(word_t *ptr) const
{
  return fun_index_.find(ptr);
}
#endif

#if FEATURE_CODE_RANGES
code::Range Module::get_code_range() {
  return code::Range(code_.data(), (&code_.back())+1);
}
#endif

} // ns gluon
