## THE NEXT <span style="color:silver">~~BIG~~</span> small THING™
# Gluon Erlang Abstract Machine

A configurable and small virtual machine which runs bytecode derived from and 
very similar to Erlang BEAM. VM runtime is rebuilt for features of your code to
be as small as possible. The goal is to go smaller than several tens of kilobytes 
(oh well, under megabyte would be cool, probably, but running in 32-64kb chip
will be a win).

## asm &mdash; assembler tool

Assembler tool which takes form of BEAM asm source (built with `erlc -S file.erl`),
parses it and writes either gluonvm binary (GLEAM) or intermediate representation
(IR).

It is designed to track features used by your code and allows you to specify those
features you really want in your VM. Error will generated if code uses something
over your requirements.

### Building asm

Dependencies: rebar3.

Run: `make asm`

## emuemu &mdash; emulator for the emulator

A high level interpreter for GluonVM intermediate representation (IR), which
simulates properties of Erlang VM (memory, instruction set, etc) and processes
(registers, stack, process execution state) and allows to run some code.

### Building emuemu

Dependencies: rebar3.

Run: `make emu`
