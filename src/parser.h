#pragma once

#include <span>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lexer.h"

struct AstNodeBasicExpr;

struct AstNodeIntLiteral {
  TokenIntLiteral value;
};
struct AstNodeStringLiteral {
  TokenStringLiteral value;
};
struct AstNodeIdentifier {
  TokenIdentifier value;
};
struct AstNodeFuncCall {
  TokenIdentifier functionName;
  std::vector<AstNodeBasicExpr> arguments;
};

struct AstNodeBasicExpr
    : public std::variant<AstNodeIntLiteral, AstNodeStringLiteral,
                          AstNodeIdentifier, AstNodeFuncCall> {};

using AstNode = AstNodeBasicExpr;

using Ast = std::vector<AstNode>;

std::string printAst(const Ast &ast);

using TokensSpan = std::span<const Token>;
Ast parseSourceFile(TokensSpan tokens);
