#pragma once

#include <span>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lexer.h"

struct AstNodeFuncCall {
  TokenIdentifier functionName;
  std::vector<TokenStringLiteral> arguments;
};

using AstNode = std::variant<AstNodeFuncCall>;
using Ast = std::vector<AstNode>;

std::string printAst(const Ast &ast);

using TokensSpan = std::span<const Token>;
Ast parseSourceFile(TokensSpan tokens);
