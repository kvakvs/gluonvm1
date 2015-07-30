#pragma once

#include "g_defs.h"
#include "g_term.h"
#include "g_error.h"
#include "g_dist.h"
#include "g_scheduler.h"

namespace gluon {

using str_atom_map_t = Map<Str, Term>;
using atom_str_map_t = Map<Term, Str>;

class Heap;
class Process;
namespace code {
  class Server;
} // ns code

// Note: singleton, do not instantiate even
class VM {
private:
  VM() = delete;

  static str_atom_map_t g_atoms;
  static atom_str_map_t g_atoms_reverse;
  static word_t         g_atom_counter;

  static Node           *g_this_node;
  // used as "" constant when atom is not found
  static Str            g_empty_str;
  static Scheduler      *g_scheduler;
  static code::Server   *g_cs;

public:
  static const word_t SLICE_REDUCTIONS = 1000; // adjust this for slow devices

  static void init();
  static code::Server *get_cs() { return g_cs; }

  //
  // Atom table
  //

  // Creates atom or returns existing
  static Term to_atom(const Str &s);
  // Returns existing or nil
  static Term to_existing_atom(const Str &s);
  static const Str &find_atom(Term a);

  //
  // Distribution
  //
  static Node *dist_this_node();

  //
  // Heap management
  //
  typedef enum {
    HEAP_VM_INTERNAL, // vm needs this for stuff
    HEAP_CODE,        // modules code goes here
    HEAP_LOADER_TMP,  // loader uses this, discard after loading
    HEAP_PROCESS      // per-process heap or something?
  } heap_t;
  static Heap *get_heap(heap_t);

  //
  // VM loop and loop labels
  //
  // this is initialized in vm_loop(nullptr) call
  static const void **g_opcode_labels;

  // Takes next process from scheduler and runs for a while, eventually switching
  // if the selected process yields or goes into receive/io wait.
  static void vm_loop(bool init);

  //
  // Bif management
  //
  //static bif0_fn resolve_bif0(mfarity_t &);
  static bif1_fn resolve_bif1(mfarity_t &);
  static bif2_fn resolve_bif2(mfarity_t &);
  static bif3_fn resolve_bif3(mfarity_t &);

  static Term apply_bif(Process *proc, mfarity_t &mfa, Term *args);

  static Scheduler *get_scheduler();

private:
  // Does not check if atom existed before. Will break old values on overwrite
  static Term new_atom(const Str &s);
  static void init_predef_atoms();
};

} // ns gluon
