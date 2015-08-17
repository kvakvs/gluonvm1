#include "g_mailbox.h"

namespace gluon {
namespace proc {

Mailbox::Mailbox() {
  m_messages.push_back(NONVALUE);
}

void Mailbox::on_incoming(Term value) {
  // Ensure there is always trailing nonvalue in msg list
  G_ASSERT(m_messages.back() == NONVALUE);
  m_messages.insert(--m_messages.end(), value);
  G_ASSERT(m_messages.back() == NONVALUE);
}

// Returns current message
// Returns NONVALUE if current points at NONVALUE terminator (after last)
// Position of pointer at mailbox end means we start over
Term Mailbox::get_current() {
  G_ASSERT(m_messages.back() == NONVALUE);

  if (m_current == m_messages.end()) {
    m_current = m_messages.begin();
  }
  if (*m_current != NONVALUE) {
    return *m_current;
  } else {
    return NONVALUE;
  }
}

// Removes current message at pointer
void Mailbox::remove_current()
{
  G_ASSERT(*m_current != NONVALUE);

  if (m_current == m_messages.end() || *m_current == NONVALUE) {
    G_FAIL("removing from empty msgbox cell");
    //m_mbox_ptr = m_mbox.begin();
  } else {
    m_messages.erase(m_current);
  }

  // Reset current position to end (auto wrapped on next read)
  m_current = m_messages.end();

  // TODO: Cancel timer
  // TODO: For off-heap message queue - free the message memory
}

// If pointer is not at terminator, step forward. Else set at mailbox end
void Mailbox::step_next()
{
  if (*m_current != NONVALUE) {
    m_current++;
  } else {
    m_current = m_messages.end();
  }
}

void Mailbox::mark_position(word_t label)
{
  m_saved_mark_label = label;
  m_saved_mark = --m_messages.end();
}

void Mailbox::set_to_marked(word_t label)
{
  if (m_saved_mark_label == label) {
    m_current = m_saved_mark;
  }
}

} // ns proc
} // ns mailbox
