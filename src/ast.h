#pragma once

#include <string>
#include <unordered_map>
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

struct AstNodeFuncRef {
    std::string generatedName;
};

struct AstNodeExpr : public std::variant<AstNodeIntLiteral, AstNodeStringLiteral, AstNodeIdentifier, AstNodeFuncCall,
                                         AstNodeAddition, AstNodeSubstraction, AstNodeNegation, AstNodeFuncRef> {};

struct AstNodeAssignment {
    TokenIdentifier variable;
    AstNodeExpr value;
};

struct AstNodeStmt : public std::variant<AstNodeAssignment, AstNodeFuncCall> {
    using std::variant<AstNodeAssignment, AstNodeFuncCall>::variant;
};

using Ast = std::vector<AstNodeStmt>;

struct TypedVariable {
    TokenIdentifier varName;
    TokenIdentifier varType;
};

struct FunctionDefinition {
    std::vector<TypedVariable> arguments;
    std::vector<TypedVariable> returnVals;
    Ast functionBody;
};

using FuncDefs = std::unordered_map<std::string, FunctionDefinition>;

std::string printAst(Ast const &ast, FuncDefs const &functions);
