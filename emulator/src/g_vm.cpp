#include "g_vm.h"
#include "g_code_server.h"
#include "platf/gsys_file.h"
#include "platf/gsys_mem.h"
#include "g_dist.h"
#include "g_heap.h"
#include "g_predef_atoms.h"
#include "g_process.h"

#include "bif/g_bif_misc.h"
#include "g_vm_bif_tab.h"
#include "g_genop.h"

#include <algorithm>

namespace gluon {

namespace err {
IMPL_EXCEPTION(FeatureMissing)
IMPL_EXCEPTION(TODO)
IMPL_EXCEPTION(BeamLoad)
IMPL_EXCEPTION(Scheduler)
IMPL_EXCEPTION(CodeServer)
IMPL_EXCEPTION(Process)
}  // ns err

VM::VM() : sched_(*this) {
  this_node_ = new Node;
  codeserver_ = new code::Server(*this);

  vm_loop(true);  // initialize labels

  init_predef_atoms();
  premade_.init(*this);

  codeserver_->path_append("/usr/lib/erlang/lib/stdlib-2.4/ebin");
  codeserver_->path_append("/usr/lib/erlang/lib/stdlib-2.5/ebin");

  codeserver_->path_append("/usr/lib/erlang/lib/erts-6.4.1/ebin");
  codeserver_->path_append("/usr/lib/erlang/lib/erts-7.0/ebin");

  codeserver_->path_append("/usr/lib/erlang/lib/xmerl-1.3.7/ebin");
  codeserver_->path_append("/usr/lib/erlang/lib/xmerl-1.3.8/ebin");

  // create root process and set it to some entry function
  root_process_ = new Process(*this, the_non_value);
  codeserver_->load_module(root_process_, atom::INIT);
  codeserver_->load_module(root_process_, atom::ERLANG);
}

RegisterResult VM::register_name(Term name, Term pid_port) {
  if (reg_names_.contains(name)) {
    return RegisterResult::RegistrationExists;
  }

  Process* p = scheduler().find(pid_port);
  if (!p) {
    return RegisterResult::ProcessNotFound;
  }
  reg_names_.insert(name, p);
  p->registered_as(name);
  return RegisterResult::Ok;
}

Term VM::to_atom(const Str& s) {
  Term a = to_existing_atom(s);
  return a.is_nil() ? new_atom(s) : a;
}

Term VM::new_atom(const Str& s) {
  Term new_a = Term::make_atom(atom_id_counter_);
  atoms_.insert(s, new_a);
  reverse_atoms_.insert(new_a, s);
  atom_id_counter_++;
  return new_a;
}

void VM::init_predef_atoms() {
  const char* p = atom::g_predef_atoms;
  atom_id_counter_ = 1;

  while (*p) {
    Word len = (Word)(p[0]);
    new_atom(Str(p + 1, len));
    p += len + 1;
  }
  // TODO: get rid of
}

const Str& VM::find_atom(Term a) const {
  G_ASSERT(a.is_atom());
  auto presult = reverse_atoms_.find_ptr(a);
  return presult ? *presult : const_empty_str_;
}

Node* VM::dist_this_node() {
#if FEATURE_ERL_DIST
  G_TODO("implement Node and this node variable")
#endif
  return this_node_;
}

// For now all heaps are located in normal C++ heap
erts::Heap* VM::get_heap(VM::heap_t) {
  return nullptr;
}

static bool find_bif_compare_fun(const bif::BIFIndex& a,
                                 const bif::BIFIndex& b) {
  return a.fun < b.fun || (a.fun == b.fun && a.arity < b.arity);
}

void* VM::find_bif(const MFArity& mfa) const {
  if (mfa.mod != atom::ERLANG) {
    return nullptr;
  }

  bif::BIFIndex sample;
  sample.fun = mfa.fun;
  sample.arity = mfa.arity;
  auto i = std::lower_bound(&bif::g_bif_table[0],
                            &bif::g_bif_table[bif::bif_table_size], sample,
                            find_bif_compare_fun);
  if (i->fun == mfa.fun && i->arity == mfa.arity) {
    return i->bif_fn;
  }
  return nullptr;
}

Term VM::apply_bif(Process* proc, MFArity& mfa, Term* args) {
  void* b = find_bif(mfa);
  if (b) {
    switch (mfa.arity) {
      case 0:
        return ((bif0_fn)b)(proc);
      case 1:
        return ((bif1_fn)b)(proc, args[0]);
      case 2:
        return ((bif2_fn)b)(proc, args[0], args[1]);
      case 3:
        return ((bif3_fn)b)(proc, args[0], args[1], args[2]);
    }  // switch
  }    // if b
  return proc->bif_error(atom::UNDEF);
}

Term VM::apply_bif(Process* proc, Word arity, void* fn, Term* args) {
  if (!fn) {
    return proc->bif_error(atom::BADFUN);
  }
  switch (arity) {
    case 0:
      return ((bif0_fn)fn)(proc);
    case 1:
      return ((bif1_fn)fn)(proc, args[0]);
    case 2:
      return ((bif2_fn)fn)(proc, args[0], args[1]);
    case 3:
      return ((bif3_fn)fn)(proc, args[0], args[1], args[2]);
  }
  return proc->bif_error(atom::UNDEF);
}

void VM::assert_jmp_address(const void* p) const {
  G_ASSERT(p >= g_opcode_labels[1]
        && p <= g_opcode_labels[genop::max_opcode]);
}

void PremadeBeaminstr::init(const VM& vm) {
  instr_[(Word)PremadeIndex::Apply_mfargs_]
      = (Word)vm.g_opcode_labels[(Word)genop::Opcode::Apply_mfargs_];

  instr_[(Word)PremadeIndex::Normal_exit_]
      = (Word)vm.g_opcode_labels[(Word)genop::Opcode::Normal_exit_];
}

}  // ns gluon
