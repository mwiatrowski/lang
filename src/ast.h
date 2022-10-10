#pragma once

#include <memory>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include "tokens.h"
#include "value_ptr.h"

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
    ValuePtr<AstNodeExpr> object;
    std::vector<AstNodeExpr> arguments;
};

struct AstNodeBinaryOp {
    Token op;
    ValuePtr<AstNodeExpr> lhs;
    ValuePtr<AstNodeExpr> rhs;
};

struct AstNodeNegation {
    ValuePtr<AstNodeExpr> operand;
};

struct AstNodeFuncRef {
    std::string generatedName;
};

struct AstNodeMemberAccess {
    ValuePtr<AstNodeExpr> object;
    TokenIdentifier member;
};

struct AstNodeExpr
    : public std::variant<AstNodeIntLiteral, AstNodeStringLiteral, AstNodeBoolLiteral, AstNodeIdentifier,
                          AstNodeFuncCall, AstNodeBinaryOp, AstNodeNegation, AstNodeFuncRef, AstNodeMemberAccess> {};

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
    ValuePtr<AstNodeStmt> brElse;
};

struct AstNodeWhileLoop {
    AstNodeExpr condition;
    ValuePtr<AstNodeStmt> body;
};

struct AstNodeBreakStmt {};
struct AstNodeContinueStmt {};

struct AstNodeStmt
    : public std::variant<AstNodeStructDecl, AstNodeDeclaration, AstNodeVarAssignment, AstNodeExpr, AstNodeScope,
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
