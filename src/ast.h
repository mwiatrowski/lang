#pragma once

#include <memory>
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

struct AstNodeBinaryOp {
    Token op;
    std::vector<AstNodeExpr> operands; // size 2
};

struct AstNodeNegation {
    std::vector<AstNodeExpr> operands; // size 1
};

struct AstNodeFuncRef {
    std::string generatedName;
};

struct AstNodeExpr : public std::variant<AstNodeIntLiteral, AstNodeStringLiteral, AstNodeIdentifier, AstNodeFuncCall,
                                         AstNodeBinaryOp, AstNodeNegation, AstNodeFuncRef> {};

struct AstNodeAssignment {
    TokenIdentifier variable;
    AstNodeExpr value;
};

struct AstNodeScope {
    StmtList statements;
};

struct Branch;
struct AstNodeIfBlock {
    std::vector<Branch> brIfElif;
    std::vector<AstNodeStmt> brElse; // size 0 or 1
};

struct AstNodeStmt : public std::variant<AstNodeAssignment, AstNodeFuncCall, AstNodeScope, AstNodeIfBlock> {
    using std::variant<AstNodeAssignment, AstNodeFuncCall, AstNodeScope, AstNodeIfBlock>::variant;
};

struct Branch {
    AstNodeExpr condition;
    AstNodeStmt body;
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
