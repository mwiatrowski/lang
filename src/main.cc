#include <filesystem>
#include <fstream>
#include <iostream>
#include <ranges>
#include <string>

#include "codegen.h"
#include "lexer.h"
#include "parser.h"
#include "strings.h"
#include "typecheck.h"

namespace {

namespace fs = std::filesystem;
namespace vws = std::views;

struct CompilationOptions {
    fs::path rootSourceFile;
    fs::path outputFile;
    bool printTokens = false;
    bool printAst = false;
};

auto toStringView(char const *cStr) { return std::string_view(cStr); }

auto parseCompilationOptions(int argc, char *argv[]) -> std::optional<CompilationOptions> {
    auto const args = std::span(argv, argv + argc) | vws::transform(toStringView);

    auto options = CompilationOptions{};

    for (auto const &arg : args | vws::drop(1)) {
        if (auto inFile = removePrefix(arg, "--input=")) {
            options.rootSourceFile = std::move(*inFile);
        } else if (auto outFile = removePrefix(arg, "--output=")) {
            options.outputFile = std::move(*outFile);
        } else if (arg == "--tokens") {
            options.printTokens = true;
        } else if (arg == "--ast") {
            options.printAst = true;
        } else {
            std::cerr << "Unknown option: '" << arg << "'" << std::endl;
        }
    }

    if (options.rootSourceFile == fs::path{} || options.outputFile == fs::path{}) {
        std::cerr << "Input or output file was not specified" << std::endl;
        return {};
    }

    return {std::move(options)};
}

void startCompilation(CompilationOptions const &options) {
    auto sourceStream = std::ifstream(options.rootSourceFile, std::ios::in);
    auto sourceFileContents =
        std::string(std::istreambuf_iterator<char>(sourceStream), std::istreambuf_iterator<char>());

    auto tokens = lexSourceCode(sourceFileContents);
    if (options.printTokens) {
        std::cout << printTokens(tokens) << std::endl;
    }

    auto parserOutput = parseSourceFile(tokens);
    if (options.printAst) {
        std::cout << printAst(parserOutput) << std::endl;
    }

    auto typeInfo = resolveTypes(parserOutput);
    (void)typeInfo;

    auto outputStream = std::ofstream(options.outputFile, std::ios::out);
    outputStream << generateCode(parserOutput);
}

} // namespace

int main(int argc, char *argv[]) {
    if (auto options = parseCompilationOptions(argc, argv)) {
        startCompilation(*options);
    }
}
