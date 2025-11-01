#!/usr/bin/env bash
./build/src/lightlisp lisp.txt
clang++ -std=c++20 -g -O0 lisp.o err.o -o lisp

./lisp
exitStatus=$?

echo "Exit status: ${exitStatus}"