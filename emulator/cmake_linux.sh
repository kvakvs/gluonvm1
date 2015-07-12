#!/bin/sh
rm -rf _build
mkdir _build
cd _build
export CC=clang
export CXX=clang++
cmake -G "Unix Makefiles" ..
