#include "g_module.h"
#include "g_reader.h"
#include "g_vm.h"
//#include "g_heap.h"

namespace gluon {


Result<word_t *> Module::resolve_function(Term f, word_t arity)
{
  printf("resolve_function(%s/%zu)\n", VM::find_atom(f).c_str(), arity);
  auto iter = m_funs.find(fun_arity_t::create(f, arity));
  if (iter == m_funs.end()) {
    return error<word_t *>("function not found");
  }
  return resolve_label(iter->second);
}

Result<word_t *> Module::resolve_label(label_index_t label)
{
  if (label.value >= m_labels.size()) {
    return error<word_t *>("label index too big");
  }
  return success(m_code.data() + m_labels[label.value]);
}

} // ns gluon
