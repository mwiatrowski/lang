#include <fstream>
#include <iostream>
#include <string>

#include "codegen.h"
#include "lexer.h"
#include "parser.h"
#include "types.h"

namespace {

constexpr auto *OUTPUT_FILE = "transpiled.cc";

void startCompilation(const std::string &rootSourceFile, const std::string &outputFile) {
    auto sourceStream = std::ifstream(rootSourceFile, std::ios::in);
    auto sourceFileContents =
        std::string(std::istreambuf_iterator<char>(sourceStream), std::istreambuf_iterator<char>());

    auto tokens = lexSourceCode(sourceFileContents);
    auto ast = parseSourceFile(tokens);
    auto typeInfo = resolveTypes(ast);
    (void)typeInfo;

    auto outputStream = std::ofstream(outputFile, std::ios::out);
    outputStream << generateCode(ast);
}

} // namespace

int main(int argc, char *argv[]) {
    if (argc != 2) {
        std::cerr << "Wrong number of arguments" << std::endl;
        return {};
    }

    auto sourceFile = std::string{argv[1]};
    std::cerr << "Compiling " << sourceFile << std::endl;

    startCompilation(sourceFile, OUTPUT_FILE);
}
