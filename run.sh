#!/usr/bin/env bash

L_GREEN='\033[1;32m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

mkdir -p build &&
cd build &&
cmake .. &&
make &&

for SOURCE in ../examples/*.lg; do
    INPUT_PATH=`realpath $SOURCE`
    FILENAME=`basename $SOURCE`
    OUTPUT_PATH="${FILENAME}.transpiled.cc"

    echo -e "${L_GREEN}=== Example: $INPUT_PATH ===${NC}"

    echo -e "${CYAN}Running the compiler${NC}" &&
    ./lang --input=${INPUT_PATH} --output=${OUTPUT_PATH} "$@" &&
    echo -e "${CYAN}Running GCC on the transpiled output${NC}" &&
    g++ "${OUTPUT_PATH}" -o compiled.out &&
    echo -e "${CYAN}Running the compiled program${NC}" &&
    ./compiled.out
done
