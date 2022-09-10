#!/usr/bin/env bash

mkdir -p build &&
cd build &&
cmake .. &&
make &&

echo "Running the compiler"
./lang ../examples/hello.lg
echo "Running GCC on the transpiled output"
g++ transpiled.cc -o compiled.out
echo "Running the compiled program"
./compiled.out
