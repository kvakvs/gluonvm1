## THE NEXT <span style="color:silver">~~BIG~~</span> small THING™
# Gluon Erlang Abstract Machine

A configurable and small virtual machine which runs Erlang BEAM bytecode. Many
simplifications have been made to keep code small at the cost of speed.
The goal is to go smaller than several tens of kilobytes (oh well, under
megabyte would be cool).

Features used in code can be configured at compile time when you want to squeeze
VM into a particularly small platform.

# The emulator

C++ implementation of minimalistic BEAM virtual machine. Located in `emulator/`.
Has simple configurable feature settings in `include/g_FEATURES.h` (like
distribution, float, bignum support etc). Note that (temporarily) some #define
are duplicated as constants.

## Building

Requires CMake, Clang (probably will work with GCC too?).

Run `make` in `emulator/` directory. `CMakeLists.txt` will also work as a project
with QtCreator IDE not to mention that CMake supports plenty of other IDEs (run
`cmake` to see list of supported IDE, see `cmake_linux.sh` to get hint how to create
a project for your IDE).

# Features (Done)

* Processes, heaps and stack (no GC yet)
* Reductions and scheduling
* Message passing and infinite receiving (no timers and timed receive yet)
* Some BIFs (ever growing amount)
* Many opcodes, BEAM file loading and code path search

*TODO*

* Exceptions
* Process links, monitors
* Simple GC
* Binaries
* Floats maybe?

# License

Apache v.2

Contributions not welcome until the project reaches POC phase (a working prototype).