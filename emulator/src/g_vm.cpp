#include "g_vm.h"
#include "g_codeserver.h"
#include "g_sys_fs.h"
#include "g_sys_mem.h"
#include "g_dist.h"
#include "g_heap.h"
#include "g_predef_atoms.h"
#include "g_process.h"
#include "bif/g_bif_misc.h"

namespace gluon {

str_atom_map_t VM::g_atoms;
atom_str_map_t VM::g_atoms_reverse;
word_t VM::g_atom_counter; // initialized in init_predef_atoms
Node *VM::g_this_node = nullptr;
const void **VM::g_opcode_labels;
Str VM::g_empty_str;
Scheduler *VM::g_scheduler = nullptr;

void VM::init()
{
  g_this_node = new Node;
  vm_loop(true); // initialize labels
  CodeServer::init();
  init_predef_atoms();
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
  return Term::make_nil();
}

Term VM::new_atom(const Str &s) {
//  auto iter = g_atoms.find(s);
//  if (iter != g_atoms.end()) {
//    G_FAIL("atom exists")
//    G_IF_NODEBUG(return Term::make_nil());
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
Heap *VM::get_heap(VM::heap_t) {
  return nullptr;
}

bif2_fn VM::resolve_bif2(mfarity_t &mfa)
{
  G_ASSERT(mfa.arity == 2);
  if (mfa.mod == atom::ERLANG) {
    if (mfa.fun == atom::Q_MINUS)        { return &bif::bif_minus_2; }
    if (mfa.fun == atom::Q_PLUS)         { return &bif::bif_plus_2; }
    if (mfa.fun == atom::Q_EQUALS)       { return &bif::bif_equals_2; }
    if (mfa.fun == atom::Q_EQUALS_EXACT) { return &bif::bif_equals_exact_2; }
    if (mfa.fun == atom::Q_LESS_EQUAL)   { return &bif::bif_less_equal_2; }
    if (mfa.fun == atom::Q_GREATER_EQUAL){ return &bif::bif_greater_equal_2; }
  }

#if G_DEBUG
  printf("ERROR bif2 undef");
  mfa.println();
#endif
  return nullptr;
}

Term VM::apply_bif(Process *proc, mfarity_t &mfa, Term *args)
{
  switch (mfa.arity) {
  case 1: {
      auto b1 = resolve_bif1(mfa);
      if (b1) {
        return b1(proc, args[0]);
      }
      break;
    }
  case 2: {
      auto b2 = resolve_bif2(mfa);
      if (b2) {
        return b2(proc, args[0], args[1]);
      }
      break;
    }
  }
  return proc->bif_error(atom::UNDEF);
}

Scheduler *VM::get_scheduler() {
  if (g_scheduler == nullptr) {
    g_scheduler = Heap::alloc_object<Scheduler>(get_heap(HEAP_VM_INTERNAL));
  }
  return g_scheduler;
}

bif1_fn VM::resolve_bif1(mfarity_t &mfa)
{
  G_ASSERT(mfa.arity == 1);
  if (mfa.mod == atom::ERLANG) {
    if (mfa.fun == atom::LENGTH)       { return &bif::bif_length_1; }
    if (mfa.fun == atom::ATOM_TO_LIST) { return &bif::bif_atom_to_list_1; }
  }

#if G_DEBUG
  printf("ERROR bif1 undef");
  mfa.println();
#endif
  return nullptr;
}

} // ns gluon
