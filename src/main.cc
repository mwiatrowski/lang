#include <fstream>
#include <iostream>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lexer.h"
#include "strings.h"

struct AstNodeFuncCall {
  std::string_view functionName;
  std::vector<std::string_view> arguments;
};

using AstNode = std::variant<AstNodeFuncCall>;
using Ast = std::vector<AstNode>;

std::string printAst(const Ast &ast) {
  auto stream = std::stringstream{};

  stream << "(" << std::endl;
  for (const auto &node : ast) {
    const auto &funcCall = std::get<AstNodeFuncCall>(node);
    stream << "\t(CALL " << funcCall.functionName;
    for (const auto &arg : funcCall.arguments) {
      stream << " " << arg;
    }
    stream << ")" << std::endl;
  }
  stream << ")" << std::endl;

  return stream.str();
}

std::optional<AstNodeFuncCall> parseFunctionCall(std::string_view line) {
  line = consumeWhitespace(line);

  auto lBrace = line.find('(');
  auto rBrace = line.find(')');
  if (lBrace == std::string_view::npos && rBrace == std::string_view::npos &&
      lBrace >= rBrace) {
    return {};
  }

  auto funcName = trimStr(line.substr(0, lBrace));

  auto args = std::vector<std::string_view>{};
  auto argsSubstr = line.substr(lBrace + 1, rBrace - lBrace - 1);
  for (const auto &arg : splitStr(argsSubstr, ',')) {
    auto trimmed = trimStr(arg);
    if (trimmed.empty()) {
      return {};
    }
    args.push_back(trimmed);
  }

  return AstNodeFuncCall{.functionName = funcName,
                         .arguments = std::move(args)};
}

Ast parseSourceFile(std::string_view source) {
  auto ast = Ast{};

  for (const auto &line : splitStr(source, '\n')) {
    if (line.empty()) {
      continue;
    }

    if (auto astNode = parseFunctionCall(line)) {
      ast.push_back(std::move(*astNode));
    } else {
      std::cerr << "Error in this line: " << line << std::endl;
    }
  }

  return ast;
}

void startCompilation(const std::string &rootSourceFile) {
  auto sourceStream = std::ifstream(rootSourceFile, std::ios::in);
  auto sourceFileContents =
      std::string(std::istreambuf_iterator<char>(sourceStream),
                  std::istreambuf_iterator<char>());

  auto tokens = lexSourceCode(sourceFileContents);
  auto ast = parseSourceFile(sourceFileContents);

  auto codeStream = std::stringstream{};
  codeStream << "#include <iostream>" << std::endl;
  codeStream << "int main() {" << std::endl;
  codeStream << "std::cout << R\"RAWSTRING(";
  codeStream << "SOURCE:\n" << sourceFileContents << std::endl;
  codeStream << "TOKENS:\n" << printTokens(tokens) << std::endl;
  codeStream << "ABSTRACT SYNTAX TREE:\n" << printAst(ast) << std::endl;
  codeStream << ")RAWSTRING\" << std::endl;" << std::endl;
  codeStream << "}" << std::endl;

  auto outputStream = std::ofstream("transpiled.cc", std::ios::out);
  outputStream << codeStream.str();
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr << "Wrong number of arguments" << std::endl;
    return {};
  }

  auto sourceFile = std::string{argv[1]};
  std::cerr << "Compiling " << sourceFile << std::endl;

  startCompilation(sourceFile);
}
