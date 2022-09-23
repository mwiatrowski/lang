#pragma once

#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include "tokens.h"

struct AstNodeExpr;
struct AstNodeStmt;

using StmtList = std::vector<AstNodeStmt>;

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

struct AstNodeScope {
    StmtList statements;
};

struct AstNodeStmt : public std::variant<AstNodeAssignment, AstNodeFuncCall, AstNodeScope> {
    using std::variant<AstNodeAssignment, AstNodeFuncCall, AstNodeScope>::variant;
};

struct TypedVariable {
    TokenIdentifier varName;
    TokenIdentifier varType;
};

struct FunctionDefinition {
    std::vector<TypedVariable> arguments;
    std::vector<TypedVariable> returnVals;
    AstNodeStmt functionBody;
};

using FuncDefs = std::unordered_map<std::string, FunctionDefinition>;

std::string printAst(StmtList const &ast, FuncDefs const &functions);
