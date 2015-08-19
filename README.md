## THE NEXT <span style="color:silver">~~BIG~~</span> small THING™
# Gluon Erlang Abstract Machine

A configurable and small virtual machine which runs Erlang BEAM bytecode. Many
simplifications have been made to keep code small at the cost of speed.
The goal is to go smaller than several tens of kilobytes (oh well, under
megabyte would be cool).

Features used in code can be configured at compile time when you want to squeeze
VM into a particularly small platform.

## emulator/

C++ implementation of minimalistic BEAM virtual machine. Has simple configurable
feature settings in `include/g_FEATURES.h` (like distribution, float, bignum support
etc).

Requires CMake, Clang (probably will work with GCC too?).

Run `make` in emulator/ directory. `CMakeLists.txt` work with QtCreator as IDE not
to mention cmake supporting host of other IDEs.

# Features (Done)

* Processes, heaps and stack (no GC yet)
* Reductions and scheduling
* Message passing and infinite receiving (no timers and timed receive yet)
* Some BIFs (ever growing amount)
* Many opcodes, BEAM file loading and code path search

# TODO

* Exceptions
* Process links, monitors
* Simple GC
* Binaries
* Floats maybe?

# Deprecated
## apps/asm, apps/emuemu and apps/prototype

Few apps which are currently out of use, but were created to help me learn BEAM universe:
A simple beam-to-something-else assembler, a high level interpreter for GluonVM 
intermediate representation (IR), which simulates properties of Erlang VM (memory,
instruction set, etc) and processes (registers, stack, process execution state).

apps/prototype is based on github.com/tonyrog/beam (owe a beer to Tony!).
