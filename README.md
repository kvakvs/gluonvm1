# GluonVM

A configurable and small virtual machine which runs own bytecode which is derived
and very similar to Erlang BEAM. VM is kept as small as possible at the cost of
speed and features (configurable per build), we're talking tens/hundreds of
kilobytes.

## asm -- assembler tool

Assembler tool which takes form of BEAM asm source (built with `erlc -S file.erl`),
parses it and writes either gluonvm binary (GLEAM) or intermediate representation
(IR).

## emuemu -- emulator emulator

A high level interpreter for GluonVM intermediate representation (IR), which
simulates properties of GluonVM (registers, stack, memory, instruction set, etc).

## Build

    $ rebar3 compile
