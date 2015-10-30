#include "g_scheduler.h"
#include "g_predef_atoms.h"
#include "g_process.h"
#include "platf/gsys_stdlib.h"

namespace gluon {

void Scheduler::add_new_runnable(Process* p) {
  G_ASSERT(p->current_queue_ == proc::Queue::None);
  G_ASSERT(p->get_pid().is_pid() == false);

  auto new_pid = Term::make_short_pid(pid_counter_++);
  p->set_pid(new_pid);

  pid_to_proc_[new_pid] = p;
  return queue_by_priority(p);
}

void Scheduler::queue_by_priority(Process* p) {
  //  G_ASSERT(!contains(m_normal_q, p));
  //  G_ASSERT(!contains(m_low_q, p));
  //  G_ASSERT(!contains(m_high_q, p));
  G_ASSERT(p->current_queue_ == proc::Queue::None);

  auto prio = p->get_priority();
  if (prio == atom::NORMAL) {
    normal_queue_.push_back(p);
    p->current_queue_ = proc::Queue::Normal;
  } else if (prio == atom::LOW) {
    low_queue_.push_back(p);
    p->current_queue_ = proc::Queue::Low;
  } else if (prio == atom::HIGH) {
    high_queue_.push_back(p);
    p->current_queue_ = proc::Queue::High;
  } else {
    throw err::Scheduler("bad prio");
  }
}

Process* Scheduler::find(Term pid) const {
  if (pid.is_pid() == false) {
    return nullptr;
  }
  auto presult = pid_to_proc_.find_ptr(pid);
  return presult ? *presult : nullptr;
}

void Scheduler::exit_process(Process* p, Term reason) {
  // assert that process is not in any queue
  G_ASSERT(p->current_queue_ == proc::Queue::None);
  // root process exits with halt()
  // G_ASSERT(p->get_registered_name() != atom::INIT);

  // TODO: ets tables
  // TODO: notify monitors
  // TODO: cancel known timers who target this process
  // TODO: notify links
  // TODO: unregister name if registered
  // TODO: if pending timers - become zombie and sit in pending timers queue
  Std::fmt("Scheduler::exit_process ");
  p->get_pid().print(vm_);
  Std::fmt("; reason=");
  reason.print(vm_);
  Std::fmt("; result X[0]=");
  p->get_runtime_ctx().regs[0].println(vm_);

  //  m_inf_wait.erase(p);
  //  m_timed_wait.erase(p);
  G_ASSERT(!contains(normal_queue_, p));
  G_ASSERT(!contains(low_queue_, p));
  G_ASSERT(!contains(high_queue_, p));
  pid_to_proc_.erase(p->get_pid());
  delete p;
}

void Scheduler::on_new_message(Process* p) {
  //  Std::fmt("sched: new message to ");
  //  p->get_pid().println();
  auto current_q = p->current_queue_;

  switch (current_q) {
    case proc::Queue::Normal:
    case proc::Queue::High:
    case proc::Queue::Low:
      return;

    case proc::Queue::InfiniteWait:
      inf_wait_.erase(p);
      break;

    case proc::Queue::TimedWait:
      throw err::TODO("timed wait new message");

    case proc::Queue::None:
      // Message arrived to a currently running process (for example send to
      // self)
      return;

    case proc::Queue::PendingTimers:
      throw err::TODO("q_pending_timers");
  }  // switch

  p->current_queue_ = proc::Queue::None;
  queue_by_priority(p);
}

Process* Scheduler::next() {
  if (current_) {
    // G_ASSERT(!contains(m_normal_q, m_current));
    // G_ASSERT(!contains(m_low_q, m_current));
    // G_ASSERT(!contains(m_high_q, m_current));
    G_ASSERT(current_->current_queue_ == proc::Queue::None);

    switch (current_->get_slice_result()) {
      case proc::SliceResult::Yield:
      case proc::SliceResult::None: {
        queue_by_priority(current_);
        current_ = nullptr;
      } break;

      case proc::SliceResult::Finished: {  // normal exit
        exit_process(current_, atom::NORMAL);
        current_ = nullptr;
      } break;

      // TODO: WAIT put into infinite or timed wait queue
      // TODO: PURGE_PROCS running on old code
      case proc::SliceResult::Exception: {
        exit_process(current_, current_->slice_result_reason_);
        current_ = nullptr;
      } break;

      case proc::SliceResult::Wait: {
        if (current_->slice_result_wait_ == proc::wait_infinite) {
          inf_wait_.insert(current_);
          current_->current_queue_ = proc::Queue::InfiniteWait;
          current_ = nullptr;
        }
      } break;
        // default:
        // G_FAIL("unknown slice result");
    }  // switch slice result
  }

  while (current_ == nullptr) {
    // TODO: monotonic clock
    // TODO: wait lists
    // TODO: network checks

    Process* next_proc = nullptr;

    // See if any are waiting in realtime (high) priority queue
    if (!high_queue_.empty()) {
      next_proc = high_queue_.front();
      high_queue_.pop_front();
    } else if (normal_count_ < NORMAL_ADVANTAGE) {
      if (!normal_queue_.empty()) {
        next_proc = normal_queue_.front();
        normal_queue_.pop_front();
      } else if (!low_queue_.empty()) {
        next_proc = low_queue_.front();
        low_queue_.pop_front();
      }
      normal_count_++;
    } else {
      if (!low_queue_.empty()) {
        next_proc = low_queue_.front();
        low_queue_.pop_front();
      } else if (!normal_queue_.empty()) {
        next_proc = normal_queue_.front();
        normal_queue_.pop_front();
      }
      normal_count_ = 0;
    }  // select proc from q

    if (next_proc) {
      Std::fmt("-----------------------------\nScheduler::next() -> ");
      Std::fmt("(Q=%d) ", (int)next_proc->current_queue_);
      next_proc->get_pid().println(vm_);

      next_proc->current_queue_ = proc::Queue::None;
      return current_ = next_proc;
    }

    // if no runnable, do some polling
    // TODO: gc waiting processes
    // TODO: check wait lists and timeouts

    // Let CPU core free if we have nothing to do
    Std::sleep(1);
  }

  throw err::TODO("should not be here");
  //  return nullptr;
}

}  // ns gluon
