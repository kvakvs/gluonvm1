## THE NEXT ~~BIG~~ small THING™
# Gluon Erlang Abstract Machine

A configurable and small virtual machine which runs bytecode derived and very
similar to Erlang BEAM. VM is kept as small as possible at the cost of
speed and features (configurable per build), the goal is to go smaller than
several tens of kilobytes (oh well, under megabyte would be cool probably.

## asm &mdash; assembler tool

Assembler tool which takes form of BEAM asm source (built with `erlc -S file.erl`),
parses it and writes either gluonvm binary (GLEAM) or intermediate representation
(IR).

It is designed to track features used by your code and allows you to specify those
features you really want in your VM. Error will generated if code uses something
over your requirements.

## emuemu &mdash; emulator for the emulator

A high level interpreter for GluonVM intermediate representation (IR), which
simulates properties of GluonVM (registers, stack, memory, instruction set, etc).

## Build

Dependencies: rebar3.

Run `rebar3 compile` or `make`.
