#!/bin/sh
rm -rf _build
mkdir _build
cd _build
export CC=clang-3.8
export CXX=clang++-3.8
cmake -G "Unix Makefiles" ..
