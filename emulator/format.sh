#!/bin/sh

BASE=`pwd`
for dir in src src/bif src/miniz src/platf include include/bif include/struct test; do
    echo "===> $dir"
    cd $BASE/$dir
    clang-format-3.8 -i *.cpp *.h *.c
done