#pragma once

#include <string>
#include <string_view>
#include <variant>
#include <vector>

struct AstNodeFuncCall {
  std::string_view functionName;
  std::vector<std::string_view> arguments;
};

using AstNode = std::variant<AstNodeFuncCall>;
using Ast = std::vector<AstNode>;

std::string printAst(const Ast &ast);

Ast parseSourceFile(std::string_view source);
