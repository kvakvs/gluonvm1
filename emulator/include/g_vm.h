#pragma once

#include "g_defs.h"
#include "g_term.h"
#include "g_error.h"
#include "g_dist.h"
#include "g_scheduler.h"

namespace gluon {

using StrAtomMap = Dict<Str, Term>;
using AtomStrMap = Dict<Term, Str>;

namespace erts {
class Heap;
}
class Process;
namespace code {
class Server;
}  // ns code

// TODO: also ports somewhere here
using AtomProcMap = Dict<Term, Process*>;

enum class RegisterResult { Ok, RegistrationExists, ProcessNotFound };

enum class PremadeIndex: Word {
  Apply_mfargs_,
  Normal_exit_,
  Total_count
};

// Contains VM code to execute one specific instruction. 'Apply' is
// used as process entry address and 'Exit' as process exit address
class PremadeBeaminstr {
public:
  Word instr_[(Word)PremadeIndex::Total_count];
  void init(const VM& vm);
};

// Note: singleton, do not instantiate even
class VM {
 private:
  // TODO: Optimize atom tab for insert-only, like OTP does
  StrAtomMap atoms_;
  AtomStrMap reverse_atoms_;
  Word atom_id_counter_;

  Node* this_node_ = nullptr;
  // used as "" constant when atom is not found
  Str const_empty_str_;
  Scheduler sched_;
  code::Server* codeserver_ = nullptr;

  // Registered names
  AtomProcMap reg_names_;
  Process* root_process_ = nullptr;

  PremadeBeaminstr premade_;

public:
  VM();

  const Word* premade_instr(PremadeIndex i) const {
    return &premade_.instr_[(Word)i];
  }
  code::Server& codeserver() { return *codeserver_; }
  const code::Server& codeserver() const { return *codeserver_; }
  Process* root_process() const { return root_process_; }

  Scheduler& scheduler() { return sched_; }
  const Scheduler& scheduler() const { return sched_; }

  //
  // Pid/port registration
  //
  RegisterResult register_name(Term name, Term pid_port);

  //
  // Atom table
  //

  // Creates atom or returns existing
  Term to_atom(const Str& s);
  // Returns existing or nil
  Term to_existing_atom(const Str& s) const {
    auto presult = atoms_.find_ptr(s);
    if (!presult) {
      return the_nil;
    }
    return *presult;
  }
  const Str& find_atom(Term a) const;

  //
  // Distribution
  //
  Node* dist_this_node();

  //
  // Heap management
  //
  typedef enum {
    HEAP_VM_INTERNAL,  // vm needs this for stuff
    HEAP_CODE,         // modules code goes here
    HEAP_LOADER_TMP,   // loader uses this, discard after loading
    HEAP_LARGE_BINARY
  } heap_t;
  erts::Heap* get_heap(heap_t);

  //
  // VM loop and loop labels
  //
  // this is initialized in vm_loop(nullptr) call
  const void** g_opcode_labels;

  // Takes next process from scheduler and runs for a while, eventually
  // switching
  // if the selected process yields or goes into receive/io wait.
  void vm_loop(bool init);

  //
  // Bif management
  //
  Term apply_bif(Process* proc, MFArity& mfa, Term* args);
  void* find_bif(const MFArity& mfa) const;
  Term apply_bif(Process* proc, Word arity, void* fn, Term* args);

  // Checks that opcode label is in allowed range and makes sense. Do not confuse
  // this with actual code address which is Word*
  void assert_jmp_address(const void* p) const;

 private:
  // Does not check if atom existed before. Will break old values on overwrite
  Term new_atom(const Str& s);
  void init_predef_atoms();
};

}  // ns gluon
