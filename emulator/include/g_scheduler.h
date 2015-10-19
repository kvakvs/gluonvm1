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
  using queue_t = List<Process *>;
  using wait_room_t = Set<Process *>;
  queue_t low_queue_;     // lowest priority (background)
  queue_t normal_queue_;  // normal priority (8x low)
  queue_t high_queue_;    // highest (realtime) priority
  wait_room_t inf_wait_;    // on infinite receive
  wait_room_t timed_wait_;  // on timed receive

  word_t           pid_counter_ = 0;
  Process         *current_     = nullptr;

  Dict<Term, Process *>  pid_to_proc_;

  //
  // Scheduling algorithm
  //

  // how often low prio gets to run despite all the business
  static constexpr word_t NORMAL_ADVANTAGE = 8;

  word_t normal_count_;

public:
  // Register process in one of queues according to its priority. New pid is set
  MaybeError add_new_runnable(Process *p);

  Process *next();
  MaybeError queue_by_priority(Process *p);
  Process *find(Term pid) const;
  void exit_process(Process *p, Term reason);
  // Wake up if process was waiting or timed-waiting
  void on_new_message(Process *p);

protected:
  inline static bool contains(queue_t &q, Process *p) {
    return std::find(q.begin(), q.end(), p) != q.end();
  }
  inline static bool contains(wait_room_t &q, Process *p) {
    return q.find(p) != q.end();
  }
};

} // ns gluon
