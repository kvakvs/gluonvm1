#include "g_module.h"
#include "g_reader.h"
#include "g_vm.h"
//#include "g_heap.h"

namespace gluon {


Result<code_offset_t> Module::resolve_function(Term f, word_t arity)
{
  auto iter = m_funs.find(fun_arity_t::create(f, arity));
  if (iter == m_funs.end()) {
    return error<code_offset_t>("function not found");
  }
  return resolve_label(iter->second);
}

Result<code_offset_t> Module::resolve_label(label_index_t label)
{
  if (label.value >= m_labels.size()) {
    return error<code_offset_t>("label index too big");
  }
  return success(m_labels[label.value]);
}

} // ns gluon
