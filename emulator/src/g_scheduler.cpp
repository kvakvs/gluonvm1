#include "g_scheduler.h"
#include "g_predef_atoms.h"
#include "g_process.h"
#include "g_sys_stdlib.h"

namespace gluon {

MaybeError Scheduler::add_runnable(Process *p)
{
  G_ASSERT(p->get_pid().is_pid() == false);

  auto new_pid = Term::make_short_pid(m_pid_counter++);
  p->set_pid(new_pid);

  return queue_by_priority(p);
}

MaybeError Scheduler::queue_by_priority(Process *p) {
  auto prio = p->get_priority();
  if (prio == atom::NORMAL) {
    m_normal_q.push(p);
  } else if (prio == atom::LOW) {
    m_low_q.push(p);
  } else if (prio == atom::HIGH) {
    m_high_q.push(p);
  } else {
    return "bad prio";
  }
  return success();
}

Process *Scheduler::next()
{
  // TODO: check previous process exit or sleep status (slice_result)

  // TODO: put back in run queue, or receive/wait queue
  if (m_current) {
    add_runnable(m_current);
    m_current = nullptr;
  }

  while (m_current == nullptr) {
    // TODO: monotonic clock
    // TODO: wait lists
    // TODO: network checks

    Process *next_proc = nullptr;

    // See if any are waiting in realtime (high) priority queue
    if (!m_high_q.empty()) {
      next_proc = m_high_q.front();
      m_high_q.pop();
    } else if (m_normal_count < NORMAL_ADVANTAGE) {
      if (!m_normal_q.empty()) {
        next_proc = m_normal_q.front();
        m_normal_q.pop();
      } else if (!m_low_q.empty()) {
        next_proc = m_low_q.front();
        m_low_q.pop();
      }
      m_normal_count++;
    } else {
      if (!m_low_q.empty()) {
        next_proc = m_low_q.front();
        m_low_q.pop();
      } else if (!m_normal_q.empty()) {
        next_proc = m_normal_q.front();
        m_normal_q.pop();
      }
      m_normal_count = 0;
    } // select proc from q

    if (next_proc) {
      return m_current = next_proc;
    }

    // if no runnable, do some polling
    // TODO: gc waiting processes
    // TODO: check wait lists and timeouts

    // Let CPU core free if we have nothing to do
    stdlib::sleep(1);
  }

  G_FAIL("should not be here");
//  return nullptr;
}


} // ns gluon
