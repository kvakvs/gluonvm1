#include "g_vm.h"
#include "g_code_server.h"
#include "g_sys_fs.h"
#include "g_sys_mem.h"
#include "g_dist.h"
#include "g_heap.h"
#include "g_predef_atoms.h"
#include "g_process.h"

#include "bif/g_bif_misc.h"
#include "g_vm_bif_tab.h"

#include <algorithm>

namespace gluon {

str_atom_map_t  VM::g_atoms;
atom_str_map_t  VM::g_atoms_reverse;
word_t          VM::g_atom_counter; // initialized in init_predef_atoms
Node           *VM::g_this_node = nullptr;
const void    **VM::g_opcode_labels;
Str             VM::g_empty_str;
Scheduler      *VM::g_scheduler = nullptr;
code::Server   *VM::g_cs = nullptr;
Process        *VM::g_root_proc = nullptr;
atom_proc_map_t VM::g_registered_names;

void VM::init()
{
  g_this_node = new Node;
  g_cs = new code::Server;

  vm_loop(true); // initialize labels

  init_predef_atoms();

  g_cs->path_append("/usr/lib/erlang/lib/stdlib-2.4/ebin");
  g_cs->path_append("/usr/lib/erlang/lib/erts-6.4.1/ebin");
  g_cs->path_append("/usr/lib/erlang/lib/xmerl-1.3.7/ebin");

  // create root process and set it to some entry function
  g_root_proc = new Process(NONVALUE);
  g_cs->load_module(g_root_proc, atom::ERLANG);
}

VM::RegResult VM::register_name(Term name, Term pid_port)
{
  auto iter = g_registered_names.find(name);
  if (iter != g_registered_names.end()) {
    return RegResult::EXISTS;
  }

  Process *p = get_scheduler()->find(pid_port);
  if (!p) {
    return RegResult::NOPROC;
  }
  g_registered_names[name] = p;
  p->registered_as(name);
  return RegResult::OK;
}

Term VM::to_atom(const Str &s)
{
  Term a = to_existing_atom(s);
  return a.is_nil() ? new_atom(s) : a;
}

Term VM::to_existing_atom(const Str &s) {
  auto iter = g_atoms.find(s);
  if (iter != g_atoms.end()) {
    return iter->second;
  }
  return NIL;
}

Term VM::new_atom(const Str &s) {
//  auto iter = g_atoms.find(s);
//  if (iter != g_atoms.end()) {
//    G_FAIL("atom exists")
//    G_IF_NODEBUG(return NIL);
//  }
  Term new_a = Term::make_atom(g_atom_counter);
  g_atoms[s]             = new_a;
  g_atoms_reverse[new_a] = s;
  g_atom_counter++;
  return new_a;
}

void VM::init_predef_atoms()
{
  const char *p = atom::g_predef_atoms;
  g_atom_counter = 1;

  while (*p) {
    word_t len = (word_t)(p[0]);
    new_atom(Str(p+1, len));
    p += len + 1;
  }

  // TODO: get rid of
}

const Str &VM::find_atom(Term a)
{
  G_ASSERT(a.is_atom());
  auto iter = g_atoms_reverse.find(a);
  if (iter != g_atoms_reverse.end()) {
    return iter->second;
  }
  return g_empty_str;
}

Node *VM::dist_this_node() {
#if FEATURE_ERL_DIST
  G_TODO("implement Node and this node variable")
#endif
  return g_this_node;
}

// For now all heaps are located in normal C++ heap
vm::Heap *VM::get_heap(VM::heap_t) {
  return nullptr;
}

static bool
find_bif_compare_fun(const bif::bif_index_t &a, const bif::bif_index_t &b) {
  return a.fun < b.fun || (a.fun == b.fun && a.arity < b.arity);
}

void *VM::find_bif(const mfarity_t &mfa)
{
  if (mfa.mod != atom::ERLANG) {
    return nullptr;
  }

  bif::bif_index_t sample;
  sample.fun = mfa.fun;
  sample.arity = mfa.arity;
  auto i = std::lower_bound(&bif::g_bif_table[0],
                           &bif::g_bif_table[bif::BIF_TABLE_SIZE],
                           sample,
                           find_bif_compare_fun);
  if (i->fun == mfa.fun && i->arity == mfa.arity) {
    return i->bif_fn;
  }
  return nullptr;
}

Term VM::apply_bif(Process *proc, mfarity_t &mfa, Term *args)
{
  void *b = find_bif(mfa);
  if (b) {
    switch (mfa.arity) {
    case 0: return ((bif0_fn)b)(proc);
    case 1: return ((bif1_fn)b)(proc, args[0]);
    case 2: return ((bif2_fn)b)(proc, args[0], args[1]);
    case 3: return ((bif3_fn)b)(proc, args[0], args[1], args[2]);
    } // switch
  } // if b
  return proc->bif_error(atom::UNDEF);
}

Term VM::apply_bif(Process *proc, word_t arity, void *fn, Term *args)
{
  if (!fn) {
    return proc->bif_error(atom::BADFUN);
  }
  switch (arity) {
  case 0: return ((bif0_fn)fn)(proc);
  case 1: return ((bif1_fn)fn)(proc, args[0]);
  case 2: return ((bif2_fn)fn)(proc, args[0], args[1]);
  case 3: return ((bif3_fn)fn)(proc, args[0], args[1], args[2]);
  }
  return proc->bif_error(atom::UNDEF);
}

Scheduler *VM::get_scheduler() {
  if (g_scheduler == nullptr) {
    g_scheduler = vm::Heap::alloc_object<Scheduler>(get_heap(HEAP_VM_INTERNAL));
  }
  return g_scheduler;
}


} // ns gluon
