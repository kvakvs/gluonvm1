#pragma once

#include "g_defs.h"
#include "g_term.h"

namespace gluon {

class Process;

namespace bif {

Term bif_self_0(Process *);
Term bif_spawn_3(Process *, Term m, Term f, Term args);
Term bif_group_leader_0(Process *);
Term bif_group_leader_2(Process *, Term pid, Term gl);
Term bif_is_process_alive_1(Process *, Term pid);
Term bif_nif_error_1(Process *, Term what);

} // ns bif
} // ns gluon
