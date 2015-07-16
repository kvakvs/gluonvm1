## THE NEXT <span style="color:silver">~~BIG~~</span> small THING™
# Gluon Erlang Abstract Machine

A configurable and small virtual machine which runs bytecode derived from and 
very similar to Erlang BEAM. The goal is to go smaller than several tens of 
kilobytes (oh well, under megabyte would be cool).

Idea of minimization is to track features used by code automatically plus allow
programmer to specify certain features he'd like to have or not have, then 
compile core VM with these features as executable or firmware. 

##  apps/asm &mdash; assembler tool

Assembler tool takes BEAM asm source (built with `erlc -S file.erl`),
parses it and writes gluonvm binary (GLEAM) which looks almost like BEAM binary
with few twists.

Building apps/asm: dependencies: rebar3. Run: `test/test.sh` to compile test module.
Then run: `make asm` to convert S source to gleam binary.

## emulator/

C++ implementation of minimalistic like-BEAM virtual machine. Has simple configurable
feature settings in `include/g_defs.h` (like distribution, float, bignum support etc).

Build with cmake (run `cmake_linux.sh`, then in `_build/` directory run `make`). 
`CMakeLists.txt` work with QtCreator as IDE not to mention cmake supporting host of
other IDEs.

## apps/emuemu and apps/prototype &mdash; attempts to make an emu prototype

A high level interpreter for GluonVM intermediate representation (IR), which
simulates properties of Erlang VM (memory, instruction set, etc) and processes
(registers, stack, process execution state) and allows to run some beam code.

apps/prototype is based on github.com/tonyrog/beam (owe a beer to Tony!).
