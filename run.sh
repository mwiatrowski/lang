#!/usr/bin/env bash

mkdir -p build &&
cd build &&
cmake .. &&
make &&

for SOURCE in ../examples/*.lg; do
    FULL_PATH=`realpath $SOURCE`
    echo "=== Example: $FULL_PATH ==="

    echo "Running the compiler" &&
    ./lang $FULL_PATH &&
    echo "Running GCC on the transpiled output" &&
    g++ transpiled.cc -o compiled.out &&
    echo "Running the compiled program" &&
    ./compiled.out
done
