#pragma once

#include "g_defs.h"
#include "term.h"

namespace gluon {

class Process;

namespace bif {

Term bif_group_leader_0(Process*);
Term bif_group_leader_2(Process*, Term pid, Term gl);
Term bif_is_process_alive_1(Process*, Term pid);
Term bif_nif_error_1(Process*, Term what);
Term bif_process_flag_2(Process* p, Term flag, Term value);
Term bif_register_2(Process*, Term name, Term pid_port);
Term bif_self_0(Process*);
Term bif_spawn_3(Process*, Term m, Term f, Term args);
Term bif_spawn_link_3(Process*, Term m, Term f, Term args);

}  // ns bif
}  // ns gluon
