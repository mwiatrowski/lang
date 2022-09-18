#pragma once

#include <string>
#include <variant>
#include <vector>

#include "tokens.h"

struct AstNodeExpr;
struct AstNodeStmt;

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

struct AstNodeFuncDef {
    std::vector<std::pair<TokenIdentifier, TokenIdentifier>> arguments;
    std::vector<std::pair<TokenIdentifier, TokenIdentifier>> returnVals;
    std::vector<AstNodeStmt> functionBody;
};

struct AstNodeExpr : public std::variant<AstNodeIntLiteral, AstNodeStringLiteral, AstNodeIdentifier, AstNodeFuncCall,
                                         AstNodeAddition, AstNodeSubstraction, AstNodeNegation, AstNodeFuncDef> {};

struct AstNodeAssignment {
    TokenIdentifier variable;
    AstNodeExpr value;
};

struct AstNodeStmt : public std::variant<AstNodeAssignment, AstNodeFuncCall> {
    using std::variant<AstNodeAssignment, AstNodeFuncCall>::variant;
};

using AstNode = AstNodeStmt;

using Ast = std::vector<AstNode>;

std::string printAst(const Ast &ast);
