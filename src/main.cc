#include <fstream>
#include <iostream>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lexer.h"
#include "parser.h"
#include "strings.h"

std::string generateFunctionCall(const AstNodeFuncCall &funcCall) {
  if (funcCall.functionName.name != "print") {
    return {};
  }

  auto stream = std::stringstream{};
  stream << "std::cout";
  for (const auto &expr : funcCall.arguments) {
    stream << " << ";

    if (std::holds_alternative<AstNodeIntLiteral>(expr)) {
      auto literal = std::get<AstNodeIntLiteral>(expr);
      stream << literal.value.value;
    } else if (std::holds_alternative<AstNodeStringLiteral>(expr)) {
      auto literal = std::get<AstNodeStringLiteral>(expr);
      stream << "\"" << literal.value.value << "\"";
    } else if (std::holds_alternative<AstNodeIdentifier>(expr)) {
      auto identifier = std::get<AstNodeIdentifier>(expr);
      stream << "\"(value of " << identifier.value.name << ")\"";
    } else if (std::holds_alternative<AstNodeFuncCall>(expr)) {
      auto funcCall = std::get<AstNodeFuncCall>(expr);
      stream << "\"(result of " << funcCall.functionName.name << ")\"";
    }

    stream << " << ' '";
  }
  stream << " << std::endl;" << std::endl;
  return stream.str();
}

void startCompilation(const std::string &rootSourceFile) {
  auto sourceStream = std::ifstream(rootSourceFile, std::ios::in);
  auto sourceFileContents =
      std::string(std::istreambuf_iterator<char>(sourceStream),
                  std::istreambuf_iterator<char>());

  auto tokens = lexSourceCode(sourceFileContents);
  auto ast = parseSourceFile(tokens);

  auto codeStream = std::stringstream{};
  codeStream << "#include <iostream>" << std::endl;
  codeStream << "int main() {" << std::endl;
  codeStream << "std::cout << R\"RAWSTRING(";
  codeStream << "SOURCE:\n" << sourceFileContents << std::endl;
  codeStream << "TOKENS:\n" << printTokens(tokens) << std::endl;
  codeStream << "ABSTRACT SYNTAX TREE:\n" << printAst(ast) << std::endl;
  codeStream << ")RAWSTRING\" << std::endl;" << std::endl;
  codeStream << "std::cout << \"EXECUTION:\" << std::endl;" << std::endl;
  for (const auto &node : ast) {
    if (std::holds_alternative<AstNodeFuncCall>(node)) {
      auto funcCall = std::get<AstNodeFuncCall>(node);
      codeStream << generateFunctionCall(funcCall) << std::endl;
    }
  }
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
