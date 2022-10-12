#pragma once

#include <memory>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include "tokens.h"
#include "value_ptr.h"

namespace ast {

struct Expr;
struct Stmt;

using StmtList = std::vector<ast::Stmt>;

struct IntLiteral {
    TokenIntLiteral value;
};
struct StringLiteral {
    TokenStringLiteral value;
};
struct BoolLiteral {
    TokenBoolLiteral value;
};
struct Identifier {
    TokenIdentifier value;
};

struct FuncCall {
    ValuePtr<Expr> object;
    std::vector<Expr> arguments;
};

struct BinaryOp {
    Token op;
    ValuePtr<Expr> lhs;
    ValuePtr<Expr> rhs;
};

struct Negation {
    ValuePtr<Expr> operand;
};

struct FuncRef {
    std::string generatedName;
};

struct MemberAccess {
    ValuePtr<Expr> object;
    TokenIdentifier member;
};

struct Expr : public std::variant<IntLiteral, StringLiteral, BoolLiteral, Identifier, FuncCall, BinaryOp, Negation,
                                  FuncRef, MemberAccess> {};

struct TypedVariable {
    TokenIdentifier varName;
    TokenIdentifier varType;
};

struct StructDef {
    std::vector<TypedVariable> members;
};

struct StructDecl {
    TokenIdentifier name;
    StructDef definition;
};

// TODO unify with variable assignment
struct Declaration {
    TokenIdentifier variable;
    TokenIdentifier type;
};

struct VarAssignment {
    Expr lhs;
    Expr rhs;
};

struct Scope {
    StmtList statements;
};

struct Branch;
struct IfBlock {
    std::vector<Branch> brIfElif;
    ValuePtr<Stmt> brElse;
};

struct WhileLoop {
    Expr condition;
    ValuePtr<Stmt> body;
};

struct BreakStmt {};
struct ContinueStmt {};

struct Stmt : public std::variant<StructDecl, Declaration, VarAssignment, Expr, Scope, IfBlock, WhileLoop, BreakStmt,
                                  ContinueStmt> {};

struct Branch {
    Expr condition;
    Stmt body;
};

struct FunctionDefinition {
    std::vector<TypedVariable> arguments;
    std::vector<TypedVariable> returnVals;
    Stmt functionBody;
};

using FuncDefs = std::unordered_map<std::string, ast::FunctionDefinition>;

} // namespace ast

struct ProgramDescription {
    ast::StmtList ast;
    ast::FuncDefs functions;
};

std::string printAst(ProgramDescription const &program);
