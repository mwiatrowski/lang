#include <filesystem>
#include <fstream>
#include <iostream>
#include <string>

#include "codegen.h"
#include "lexer.h"
#include "parser.h"
#include "types.h"

namespace {

void startCompilation(const std::filesystem::path &rootSourceFile, const std::filesystem::path &outputFile) {
    auto sourceStream = std::ifstream(rootSourceFile, std::ios::in);
    auto sourceFileContents =
        std::string(std::istreambuf_iterator<char>(sourceStream), std::istreambuf_iterator<char>());

    auto tokens = lexSourceCode(sourceFileContents);
    auto parserOutput = parseSourceFile(tokens);
    auto typeInfo = resolveTypes(parserOutput);
    (void)typeInfo;

    auto outputStream = std::ofstream(outputFile, std::ios::out);
    outputStream << generateCode(parserOutput.ast);
}

} // namespace

int main(int argc, char *argv[]) {
    if (argc != 2) {
        std::cerr << "Wrong number of arguments" << std::endl;
        return {};
    }

    auto sourceFile = std::filesystem::path{argv[1]};
    std::cerr << "Compiling " << sourceFile << std::endl;

    auto outputFile = sourceFile.filename().string() + ".transpiled.cc";
    startCompilation(sourceFile, outputFile);
}
