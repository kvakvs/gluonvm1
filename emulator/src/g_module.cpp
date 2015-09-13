#include "g_module.h"
#include "g_reader.h"
#include "g_vm.h"
//#include "g_heap.h"

namespace gluon {


export_t *Module::find_export(const fun_arity_t &fa)
{
  auto iter = m_exports.find(fa);
  if (iter == m_exports.end()) {
    return nullptr;
  }
  return &iter->second;
}

// TODO: duplicates find_export, replace with fun table search or remove?
//Result<export_t *> Module::resolve_function(Term f, word_t arity)
//{
//  auto iter = m_exports.find(fun_arity_t(f, arity));
//  if (iter == m_exports.end()) {
//    return error<word_t *>("function not found");
//  }
//  return success(&iter->second);
//}

Result<word_t *> Module::resolve_label(label_index_t label)
{
  if (label.value >= m_labels.size()) {
    return error<word_t *>("label index too big");
  }
  return success(m_labels[label.value]);
}

fun_arity_t Module::find_fun_arity(word_t *ptr) const
{
  return m_fun_index.find(ptr);
}

#if FEATURE_CODE_RANGES
code::Range Module::get_code_range() {
  return code::Range(m_code.data(), (&m_code.back())+1);
}
#endif

} // ns gluon
