#!/bin/sh

erlc -S init.erl
erlc init.erl
#gdb ../_build/gluonvm

