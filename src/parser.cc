#include "parser.h"

#include <iostream>
#include <optional>
#include <sstream>

#include "strings.h"

namespace {

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

} // namespace

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
