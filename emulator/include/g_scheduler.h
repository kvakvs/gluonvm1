#pragma once

#include "g_defs.h"
#include "g_error.h"
#include "g_term.h"
#include "struct/g_dict.h"

#include <algorithm>

namespace gluon {

class Process;

// A single core process scheduler with timers, queues and stuff
class Scheduler {
private:
  using Queue = List<Process *>;
  using WaitRoom = Set<Process *>;

  VM      &vm_;
  Queue low_queue_;     // lowest priority (background)
  Queue normal_queue_;  // normal priority (8x low)
  Queue high_queue_;    // highest (realtime) priority
  WaitRoom inf_wait_;    // on infinite receive
  WaitRoom timed_wait_;  // on timed receive

  Word      pid_counter_  = 0;
  Process   *current_     = nullptr;

  Dict<Term, Process *>  pid_to_proc_;

  //
  // Scheduling algorithm
  //

  // how often low prio gets to run despite all the business
  static constexpr Word NORMAL_ADVANTAGE = 8;

  Word normal_count_ = 0;

public:
  Scheduler(VM &vm): vm_(vm) {}

  // Register process in one of queues according to its priority. New pid is set
  void add_new_runnable(Process *p);

  Process *next();
  void queue_by_priority(Process *p);
  Process *find(Term pid) const;
  void exit_process(Process *p, Term reason);
  // Wake up if process was waiting or timed-waiting
  void on_new_message(Process *p);

protected:
  inline static bool contains(Queue &q, Process *p) {
    return std::find(q.begin(), q.end(), p) != q.end();
  }
  inline static bool contains(WaitRoom &q, Process *p) {
    return q.find(p) != q.end();
  }
};

} // ns gluon
