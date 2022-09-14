#pragma once

#include <span>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lexer.h"

struct AstNodeExpr;

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
  std::vector<AstNodeExpr> arguments;
};

struct AstNodeAddition {
  std::vector<AstNodeExpr> operands; // size 2
};

struct AstNodeSubstraction {
  std::vector<AstNodeExpr> operands; // size 2
};

struct AstNodeNegation {
  std::vector<AstNodeExpr> operands; // size 1
};

struct AstNodeExpr
    : public std::variant<AstNodeIntLiteral, AstNodeStringLiteral,
                          AstNodeIdentifier, AstNodeFuncCall, AstNodeAddition,
                          AstNodeSubstraction, AstNodeNegation> {};

using AstNode = AstNodeExpr;

using Ast = std::vector<AstNode>;

std::string printAst(const Ast &ast);

using TokensSpan = std::span<const Token>;
Ast parseSourceFile(TokensSpan tokens);
