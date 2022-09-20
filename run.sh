#!/usr/bin/env bash

L_GREEN='\033[1;32m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

mkdir -p build &&
cd build &&
cmake .. &&
make &&

for SOURCE in ../examples/*.lg; do
    FULL_PATH=`realpath $SOURCE`
    FILENAME=`basename $SOURCE`

    echo -e "${L_GREEN}=== Example: $FULL_PATH ===${NC}"

    echo -e "${CYAN}Running the compiler${NC}" &&
    ./lang $FULL_PATH &&
    echo -e "${CYAN}Running GCC on the transpiled output${NC}" &&
    g++ "${FILENAME}.transpiled.cc" -o compiled.out &&
    echo -e "${CYAN}Running the compiled program${NC}" &&
    ./compiled.out
done
