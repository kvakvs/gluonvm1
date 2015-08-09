#pragma once

#include "g_defs.h"
#include "g_term.h"

namespace gluon {

class Process;

namespace bif {

Term bif_self_0(Process *);
Term bif_spawn_3(Process *, Term m, Term f, Term args);

} // ns bif
} // ns gluon
