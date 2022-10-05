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
struct AstNodeBoolLiteral {
    TokenBoolLiteral value;
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

struct AstNodeExpr : public std::variant<AstNodeIntLiteral, AstNodeStringLiteral, AstNodeBoolLiteral, AstNodeIdentifier,
                                         AstNodeFuncCall, AstNodeBinaryOp, AstNodeNegation, AstNodeFuncRef> {};

struct TypedVariable {
    TokenIdentifier varName;
    TokenIdentifier varType;
};

struct AstNodeStructDef {
    std::vector<TypedVariable> members;
};

struct AstNodeStructDecl {
    TokenIdentifier name;
    AstNodeStructDef definition;
};

// TODO unify with variable assignment
struct AstNodeDeclaration {
    TokenIdentifier variable;
    TokenIdentifier type;
};

struct AstNodeVarAssignment {
    AstNodeExpr lhs;
    AstNodeExpr rhs;
};

struct AstNodeScope {
    StmtList statements;
};

struct Branch;
struct AstNodeIfBlock {
    std::vector<Branch> brIfElif;
    std::vector<AstNodeStmt> brElse; // size 0 or 1
};

struct AstNodeWhileLoop {
    AstNodeExpr condition;
    std::vector<AstNodeStmt> body; // size 1
};

struct AstNodeBreakStmt {};
struct AstNodeContinueStmt {};

struct AstNodeStmt
    : public std::variant<AstNodeStructDecl, AstNodeDeclaration, AstNodeVarAssignment, AstNodeFuncCall, AstNodeScope,
                          AstNodeIfBlock, AstNodeWhileLoop, AstNodeBreakStmt, AstNodeContinueStmt> {};

struct Branch {
    AstNodeExpr condition;
    AstNodeStmt body;
};

struct FunctionDefinition {
    std::vector<TypedVariable> arguments;
    std::vector<TypedVariable> returnVals;
    AstNodeStmt functionBody;
};

using FuncDefs = std::unordered_map<std::string, FunctionDefinition>;

struct ProgramDescription {
    StmtList ast;
    FuncDefs functions;
};

std::string printAst(ProgramDescription const &program);
