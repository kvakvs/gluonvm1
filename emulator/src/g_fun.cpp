#include "g_fun.h"
#include <string.h>

namespace gluon {
namespace fun {

boxed_fun_t *box_fun(fun_entry_t *fe, word_t *mem, Term pid, Term *frozen)
{
  boxed_fun_t *bf = (boxed_fun_t *)mem;
  // pack nfree and arity, then create_subtag() will shift it and tag as boxedfun
  bf->hdr = term_tag::BoxedFun::create_subtag(fe->num_free << 12 | fe->mfa.arity);

  G_ASSERT(pid.is_pid());
  bf->pid = pid;
  bf->module = fe->mfa.mod;
  bf->index = fe->index;
  ::memcpy(&bf->uniq, fe->uniq, sizeof(u32_t) * 4);
  bf->old_index = fe->old_index;
  bf->old_uniq = fe->old_uniq;
  bf->fe = fe;
  ::memcpy(bf->frozen, frozen, fe->num_free * sizeof(Term));

  return bf;
}

} // ns fun
} // ns gluon
