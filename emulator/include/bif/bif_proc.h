#pragma once

#include "defs.h"
#include "term.h"

namespace gluon {

class Process;

namespace bif {

// E
Term bif_exit_1(Process* proc, Term what);
Term bif_exit_2(Process* proc, Term pid, Term what);

// G
Term bif_group_leader_0(Process*);
Term bif_group_leader_2(Process*, Term pid, Term gl);

// I
Term bif_is_process_alive_1(Process*, Term pid);

// N
Term bif_nif_error_1(Process*, Term what);

// P
Term bif_process_flag_2(Process* p, Term flag, Term value);

// R
Term bif_register_2(Process*, Term name, Term pid_port);

// S
Term bif_self_0(Process*);
Term bif_spawn_3(Process*, Term m, Term f, Term args);
Term bif_spawn_link_3(Process*, Term m, Term f, Term args);

}  // ns bif
}  // ns gluon
