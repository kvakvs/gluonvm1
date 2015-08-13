#pragma once

#include "g_defs.h"
#include "g_error.h"
#include "g_term.h"

#include <algorithm>

namespace gluon {

class Process;

// A single core process scheduler with timers, queues and stuff
class Scheduler {
private:
  using queue_t = List<Process *>;
  using wait_room_t = Set<Process *>;
  queue_t m_low_q;     // lowest priority (background)
  queue_t m_normal_q;  // normal priority (8x low)
  queue_t m_high_q;    // highest (realtime) priority
  wait_room_t m_inf_wait;    // on infinite receive
  wait_room_t m_timed_wait;  // on timed receive

  word_t           m_pid_counter = 0;
  Process         *m_current     = nullptr;

  Map<Term, Process *>  m_pid_to_proc;

  //
  // Scheduling algorithm
  //
  const word_t NORMAL_ADVANTAGE = 8;  // how often low prio gets to run
  word_t m_normal_count;

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
