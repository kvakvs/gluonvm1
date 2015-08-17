#pragma once

#include "g_defs.h"
#include "g_term.h"

namespace gluon {
namespace proc {

//
// Mailbox
//
// Stuff arrives here, TODO: make this on process heap
// Requirements to data structure:
// *  step forward
// *  ability to address any cell via iterator or a pointer
// *  ability to remove arbitrary item pointed at with iterator
// *  ability to go 1 step beyond the end (zero mark)
class Mailbox {
private:
  using mailbox_t = List<Term>;
  using c_iterator_t = mailbox_t::const_iterator;

  mailbox_t     m_messages;
  c_iterator_t  m_current = m_messages.end();

  // Set by recv_mark opcode and read by recv_set opcode
  word_t        m_saved_mark_label;
  c_iterator_t  m_saved_mark;

public:
  Mailbox();

  void on_incoming(Term value);
  Term get_current();
  void remove_current();
  void step_next();
  void mark_position(word_t label);
  void set_to_marked(word_t label);

};

} // ns proc
} // ns mailbox
