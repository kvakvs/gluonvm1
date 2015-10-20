#include "g_fun.h"
#include <string.h>

#include "g_heap.h"

namespace gluon {
namespace fun {

boxed_fun_t *box_fun(fun_entry_t *fe, word_t *mem, Term pid, Term *frozen)
{
  boxed_fun_t *bf = (boxed_fun_t *)mem;
  // pack nfree and arity, then create_subtag() will shift it and tag as boxedfun
  bf->hdr = term_tag::BoxedFun::create_subtag((fe->num_free << 8)|fe->mfa.arity);

  G_ASSERT(pid.is_pid());
  bf->pid = pid;
  bf->module = fe->mfa.mod;
  bf->index = fe->index;
  std::copy(fe->uniq, fe->uniq+4, bf->uniq);
  //::memcpy(&bf->uniq, fe->uniq, sizeof(u32_t) * 4);
  bf->old_index = fe->old_index;
  bf->old_uniq = fe->old_uniq;
  bf->fun_entry = fe;
  std::copy(frozen, frozen + fe->num_free, bf->frozen);
  //::memcpy(bf->frozen, frozen, fe->num_free * sizeof(Term));

  return bf;
}

Term box_fun(proc::Heap *heap, fun_entry_t *fe, Term pid, Term *frozen)
{
  word_t *p8 = heap->allocate<word_t>(
        calculate_word_size(sizeof(boxed_fun_t) + fe->num_free)
        );

  boxed_fun_t *p = fun::box_fun(fe, p8, pid, frozen);

  return FunObject::make(p);
}

} // ns fun
} // ns gluon
