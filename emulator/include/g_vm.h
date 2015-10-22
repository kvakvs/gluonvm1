#pragma once

#include "g_defs.h"
#include "g_term.h"
#include "g_error.h"
#include "g_dist.h"
#include "g_scheduler.h"

namespace gluon {

using str_atom_map_t = Dict<Str, Term>;
using atom_str_map_t = Dict<Term, Str>;

namespace erts {
  class Heap;
}
class Process;
namespace code {
  class Server;
} // ns code

// TODO: also ports somewhere here
using atom_proc_map_t = Dict<Term, Process *>;

// Note: singleton, do not instantiate even
class VM {
private:
  // TODO: Optimize atom tab for insert-only, like OTP does
  str_atom_map_t atoms_;
  atom_str_map_t reverse_atoms_;
  word_t         atom_id_counter_;

  Node           *this_node_ = nullptr;
  // used as "" constant when atom is not found
  Str            const_empty_str_;
  Scheduler      sched_;
  code::Server   *codeserver_ = nullptr;

  // Registered names
  atom_proc_map_t  reg_names_;
  Process *root_process_ = nullptr;

public:
  VM();

  code::Server &codeserver() { return *codeserver_; }
  const code::Server &codeserver() const { return *codeserver_; }
  Process *root_process() const { return root_process_; }

  Scheduler &scheduler() { return sched_; }
  const Scheduler &scheduler() const { return sched_; }

  //
  // Pid/port registration
  //
  enum class RegResult { OK, EXISTS, NOPROC };
  RegResult register_name(Term name, Term pid_port);

  //
  // Atom table
  //

  // Creates atom or returns existing
  Term to_atom(const Str &s);
  // Returns existing or nil
  Term to_existing_atom(const Str &s) {
    return atoms_.find_ref(s, NIL);
  }
  const Str &find_atom(Term a) const;

  //
  // Distribution
  //
  Node *dist_this_node();

  //
  // Heap management
  //
  typedef enum {
    HEAP_VM_INTERNAL, // vm needs this for stuff
    HEAP_CODE,        // modules code goes here
    HEAP_LOADER_TMP,  // loader uses this, discard after loading
    HEAP_LARGE_BINARY
  } heap_t;
  erts::Heap *get_heap(heap_t);

  //
  // VM loop and loop labels
  //
  // this is initialized in vm_loop(nullptr) call
  const void **g_opcode_labels;

  // Takes next process from scheduler and runs for a while, eventually switching
  // if the selected process yields or goes into receive/io wait.
  void vm_loop(bool init);

  //
  // Bif management
  //
  Term apply_bif(Process *proc, mfarity_t &mfa, Term *args);
  void *find_bif(const mfarity_t &mfa) const;
  Term apply_bif(Process *proc, word_t arity, void *fn, Term *args);

private:
  // Does not check if atom existed before. Will break old values on overwrite
  Term new_atom(const Str &s);
  void init_predef_atoms();
};

} // ns gluon
