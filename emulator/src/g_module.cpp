#include "g_module.h"
#include "g_reader.h"
#include "g_vm.h"
//#include "g_heap.h"

namespace gluon {


Result<word_t *> Module::resolve_function(Term f, word_t arity)
{
  f.println();

  auto iter = m_exports.find(fun_arity_t(f, arity));
  if (iter == m_exports.end()) {
    return error<word_t *>("function not found");
  }
  //const fun_entry_t &fe = iter->second;
  //G_ASSERT(fe.code);
  //return success(fe.code);
  return success(iter->second);
}

Result<word_t *> Module::resolve_label(label_index_t label)
{
  if (label.value >= m_labels.size()) {
    return error<word_t *>("label index too big");
  }
  return success(m_labels[label.value]);
}

} // ns gluon
