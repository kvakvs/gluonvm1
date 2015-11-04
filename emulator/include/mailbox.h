#pragma once

#include "defs.h"
#include "term.h"

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
  using MailboxStorage = List<Term>;
  using ConstIterator = MailboxStorage::const_iterator;

  MailboxStorage messages_;
  ConstIterator current_ = messages_.end();

  // Set by recv_mark opcode and read by recv_set opcode
  Word saved_mark_label_;
  ConstIterator saved_mark_;

 public:
  Mailbox();

  void on_incoming(Term value);
  Term get_current();
  void remove_current();
  void step_next();
  void mark_position(Word label);
  void set_to_marked(Word label);
};

}  // ns proc
}  // ns mailbox
