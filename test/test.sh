#!/bin/sh

for f in `ls *.erl`; do
    erlc -S $f.erl
    erlc $f.erl
done
#gdb ../_build/gluonvm

