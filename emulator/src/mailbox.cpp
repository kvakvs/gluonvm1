#include "mailbox.h"

namespace gluon {
namespace proc {

Mailbox::Mailbox() {
    messages_.push_back(the_non_value);
}

void Mailbox::on_incoming(Term value) {
    // Ensure there is always trailing nonvalue in msg list
    G_ASSERT(messages_.back() == the_non_value);
    messages_.insert(--messages_.end(), value);
    G_ASSERT(messages_.back() == the_non_value);

    // Reset save (current) position to beginning
    current_ = messages_.begin();
}

// Returns current message
// Returns NONVALUE if current points at NONVALUE terminator (after last)
// Position of pointer at mailbox end means we start over
Term Mailbox::get_current() {
    G_ASSERT(messages_.back() == the_non_value);

    if (current_ == messages_.end()) {
        current_ = messages_.begin();
    }
    if (*current_ != the_non_value) {
        return *current_;
    } else {
        return the_non_value;
    }
}

// Removes current message at pointer
void Mailbox::remove_current() {
    G_ASSERT(*current_ != the_non_value);

    if (current_ == messages_.end() || *current_ == the_non_value) {
        throw err::Process("removing from empty msgbox cell");
        // m_mbox_ptr = m_mbox.begin();
    } else {
        messages_.erase(current_);
    }

    // Reset current position to begin
    current_ = messages_.begin();

    // TODO: Cancel timer
    // TODO: For off-heap message queue - free the message memory
}

// If pointer is not at terminator, step forward. Else set at mailbox end
void Mailbox::step_next() {
    if (*current_ != the_non_value) {
        current_++;
    } else {
        current_ = messages_.end();
    }
}

void Mailbox::mark_position(Word label) {
    saved_mark_label_ = label;
    saved_mark_ = --messages_.end();
}

void Mailbox::set_to_marked(Word label) {
    if (saved_mark_label_ == label) {
        current_ = saved_mark_;
    }
}

}  // ns proc
}  // ns mailbox
