#include "g_scheduler.h"
#include "g_predef_atoms.h"
#include "g_process.h"
#include "g_sys_stdlib.h"

namespace gluon {

MaybeError Scheduler::add_new_runnable(Process *p)
{
  G_ASSERT(p->get_pid().is_pid() == false);

  auto new_pid = Term::make_short_pid(m_pid_counter++);
  p->set_pid(new_pid);

  m_pid_to_proc[new_pid] = p;
  return queue_by_priority(p);
}

MaybeError Scheduler::queue_by_priority(Process *p) {
  G_ASSERT(!contains(m_normal_q, p));
  G_ASSERT(!contains(m_low_q, p));
  G_ASSERT(!contains(m_high_q, p));

  auto prio = p->get_priority();
  if (prio == atom::NORMAL) {
    m_normal_q.push_back(p);
    p->m_current_queue = proc::Q_NORMAL;
  } else if (prio == atom::LOW) {
    m_low_q.push_back(p);
    p->m_current_queue = proc::Q_LOW;
  } else if (prio == atom::HIGH) {
    m_high_q.push_back(p);
    p->m_current_queue = proc::Q_HIGH;
  } else {
    return "bad prio";
  }
  return success();
}

Process *Scheduler::find(Term pid) const
{
  if (pid.is_pid() == false) {
    return nullptr;
  }
  auto i = m_pid_to_proc.find(pid);
  if (i == m_pid_to_proc.end()) {
    return nullptr;
  }
  return i->second;
}

void Scheduler::exit_process(Process *p, Term reason)
{
  // assert that process is not in any queue
  G_ASSERT(p->m_current_queue == proc::Q_NONE);
  // root process exits with halt()
  //G_ASSERT(p->get_registered_name() != atom::INIT);

  // TODO: ets tables
  // TODO: notify monitors
  // TODO: cancel known timers who target this process
  // TODO: notify links
  // TODO: unregister name if registered
  // TODO: if pending timers - become zombie and sit in pending timers queue
  printf("Scheduler::exit_process ");
  p->get_pid().print();
  printf("; reason=");
  reason.print();
  printf("; result X[0]=");
  p->get_runtime_ctx().regs[0].println();

//  m_inf_wait.erase(p);
//  m_timed_wait.erase(p);
  G_ASSERT(!contains(m_normal_q, p));
  G_ASSERT(!contains(m_low_q, p));
  G_ASSERT(!contains(m_high_q, p));
  m_pid_to_proc.erase(p->get_pid());
  delete p;
}

void Scheduler::on_new_message(Process *p)
{
//  printf("sched: new message to ");
//  p->get_pid().println();
  auto current_q = p->m_current_queue;

  if (current_q == proc::Q_NORMAL ||
      current_q == proc::Q_HIGH ||
      current_q == proc::Q_LOW) {
    return;
  }

  if (current_q == proc::Q_INF_WAIT) {
    m_inf_wait.erase(p);
  } else if (current_q == proc::Q_TIMED_WAIT) {
    G_TODO("timed wait new message");
  }
  p->m_current_queue = proc::Q_NONE;
  queue_by_priority(p);
}

Process *Scheduler::next()
{
  if (m_current) {
    G_ASSERT(m_current->m_current_queue == proc::Q_NONE);

    switch (m_current->get_slice_result()) {
    case proc::SR_YIELD:
    case proc::SR_NONE: {
        queue_by_priority(m_current);
        m_current = nullptr;
      } break;

    case proc::SR_FINISHED: { // normal exit
        exit_process(m_current, atom::NORMAL);
        m_current = nullptr;
      } break;

      // TODO: WAIT put into infinite or timed wait queue
      // TODO: PURGE_PROCS running on old code
    case proc::SR_EXCEPTION: {
        exit_process(m_current, m_current->m_slice_result_reason);
        m_current = nullptr;
      } break;

    case proc::SR_WAIT: {
        if (m_current->m_slice_result_wait == proc::WAIT_INFINITE) {
          m_inf_wait.insert(m_current);
          m_current->m_current_queue = proc::Q_INF_WAIT;
          m_current = nullptr;
        }
      } break;
    //default:
      // G_FAIL("unknown slice result");
    } // switch slice result
  }

  while (m_current == nullptr) {
    // TODO: monotonic clock
    // TODO: wait lists
    // TODO: network checks

    Process *next_proc = nullptr;

    // See if any are waiting in realtime (high) priority queue
    if (         !m_high_q.empty()) {
      next_proc = m_high_q.front();
                  m_high_q.pop_front();
    } else if (m_normal_count < NORMAL_ADVANTAGE) {
      if (         !m_normal_q.empty()) {
        next_proc = m_normal_q.front();
                    m_normal_q.pop_front();
      } else if (  !m_low_q.empty()) {
        next_proc = m_low_q.front();
                    m_low_q.pop_front();
      }
      m_normal_count++;
    } else {
      if (         !m_low_q.empty()) {
        next_proc = m_low_q.front();
                    m_low_q.pop_front();
      } else if (  !m_normal_q.empty()) {
        next_proc = m_normal_q.front();
                    m_normal_q.pop_front();
      }
      m_normal_count = 0;
    } // select proc from q

    if (next_proc) {
      printf("Scheduler::next() -> ");
      printf("(Q=%d) ", (int)next_proc->m_current_queue);
      next_proc->get_pid().println();

      next_proc->m_current_queue = proc::Q_NONE;
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
