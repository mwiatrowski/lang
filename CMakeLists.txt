cmake_minimum_required(VERSION 3.1)
project (lang_tbd CXX)

set(CMAKE_CXX_STANDARD 20)

add_compile_options(-g)
# add_compile_options(-O3)

add_compile_options(-Wall -Wextra -pedantic -Werror)

add_compile_options(-fsanitize=address)
add_link_options(-fsanitize=address)

set(COMPILER_EXECUTABLE "lang")
add_executable(${COMPILER_EXECUTABLE}
    src/lexer.cc
    src/main.cc
    src/parser.cc
    src/strings.cc
    src/types.cc
)
