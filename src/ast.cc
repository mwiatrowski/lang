#include "ast.h"

#include <cassert>
#include <iostream>
#include <sstream>
#include <string>

#include "variant_helpers.h"

namespace {

std::string printStatement(const ast::Stmt &stmt, const ast::FuncDefs &functions);

std::string printExpression(const ast::Expr &expr, const ast::FuncDefs &functions) {
    if (std::holds_alternative<ast::IntLiteral>(expr)) {
        auto literal = std::get<ast::IntLiteral>(expr);
        return "(INT_LITERAL " + std::to_string(literal.value.value) + ")";
    } else if (std::holds_alternative<ast::StringLiteral>(expr)) {
        auto literal = std::get<ast::StringLiteral>(expr);
        return "(STRING_LITERAL " + std::string{literal.value.value} + ")";
    } else if (is<ast::BoolLiteral>(expr)) {
        auto const &literal = as<ast::BoolLiteral>(expr);
        return "(BOOL_LITERAL " + std::string{literal.value.value ? "true" : "false"} + ")";
    } else if (std::holds_alternative<ast::Identifier>(expr)) {
        auto identifier = std::get<ast::Identifier>(expr);
        return "(IDENTIFIER " + std::string{identifier.value.name} + ")";
    } else if (is<ast::FuncCall>(expr)) {
        auto const &funcCall = as<ast::FuncCall>(expr);
        auto stream = std::stringstream{};
        stream << "(CALL " << printExpression(*funcCall.object, functions) << " (";
        for (const auto &arg : funcCall.arguments) {
            stream << " " << printExpression(arg, functions);
        }
        stream << " ))";
        return stream.str();
    } else if (auto binaryOp = to<ast::BinaryOp>(expr)) {
        return "(" + printExpression(*binaryOp->lhs, functions) + " " + printToken(binaryOp->op) + " " +
               printExpression(*binaryOp->rhs, functions) + ")";
    } else if (std::holds_alternative<ast::Negation>(expr)) {
        auto negation = std::get<ast::Negation>(expr);
        return "( - " + printExpression(*negation.operand, functions) + ")";
    } else if (const auto funcRef = to<ast::FuncRef>(expr)) {
        auto it = functions.find(funcRef->generatedName);
        assert(it != functions.end());
        const auto &funcDef = it->second;

        auto out = std::stringstream{};
        out << "(FUNCTION ARGS (";
        for (const auto &[argName, argType] : funcDef.arguments) {
            out << " (ARG " << argName.name << " TYPE " << argType.name << ")";
        }
        out << ") RETURN_VALUES (";
        for (const auto &[retName, retType] : funcDef.returnVals) {
            out << " (RET_VAL " << retName.name << " TYPE " << retType.name << ")";
        }
        out << ") BEGIN\n";
        out << printStatement(funcDef.functionBody, functions) << "\n";
        out << "END";
        return out.str();
    } else if (is<ast::MemberAccess>(expr)) {
        auto const &memAcc = as<ast::MemberAccess>(expr);
        return "(FROM " + printExpression(*memAcc.object, functions) + " GET " + std::string{memAcc.member.name} + ")";
    }

    std::cerr << "Unexpected expression type! Index: " << expr.index() << std::endl;
    assert(false);
}

std::string printStatement(const ast::Stmt &stmt, const ast::FuncDefs &functions) {
    if (is<ast::Expr>(stmt)) {
        auto const &expr = as<ast::Expr>(stmt);
        return printExpression(expr, functions);
    }

    if (is<ast::StructDecl>(stmt)) {
        auto const &structDecl = to<ast::StructDecl>(stmt);

        auto out = std::stringstream{};

        out << "(STRUCT " << structDecl->name.name << "\n";
        for (auto const &[memberName, memberType] : structDecl->definition.members) {
            out << "MEMBER " << memberName.name << " TYPE " << memberType.name << '\n';
        }
        out << ")";

        return out.str();
    }

    if (is<ast::Declaration>(stmt)) {
        auto const &decl = as<ast::Declaration>(stmt);
        return std::string{decl.variable.name} + " HAS_TYPE " + std::string{decl.type.name};
    }

    if (std::holds_alternative<ast::VarAssignment>(stmt)) {
        auto assignment = std::get<ast::VarAssignment>(stmt);
        return printExpression(assignment.lhs, functions) + " := " + printExpression(assignment.rhs, functions);
    }

    if (const auto scope = to<ast::Scope>(stmt)) {
        auto out = std::stringstream{};
        out << "{\n";
        for (const auto &subStmt : scope->statements) {
            out << printStatement(subStmt, functions) << "\n";
        }
        out << "}";
        return out.str();
    }

    if (is<ast::IfBlock>(stmt)) {
        auto const &ifBlock = as<ast::IfBlock>(stmt);

        auto out = std::stringstream{};

        bool isFirst = true;
        for (auto const &branch : ifBlock.brIfElif) {
            out << (isFirst ? "IF" : "ELIF") << " (" << printExpression(branch.condition, functions) << ") {\n"
                << printStatement(branch.body, functions) << "\n}\n";
            isFirst = false;
        }

        if (ifBlock.brElse.hasValue()) {
            auto const &elseBody = *ifBlock.brElse;
            out << "ELSE {\n" << printStatement(elseBody, functions) << "\n}\n";
        }

        return out.str();
    }

    if (is<ast::WhileLoop>(stmt)) {
        auto const &loop = as<ast::WhileLoop>(stmt);
        return "WHILE (" + printExpression(loop.condition, functions) + ") {\n" +
               printStatement(*loop.body, functions) + "\n}";
    }

    if (is<ast::BreakStmt>(stmt)) {
        return "BREAK";
    }

    if (is<ast::ContinueStmt>(stmt)) {
        return "CONTINUE";
    }

    std::cerr << "Unexpected statement type! Index: " << stmt.index() << std::endl;
    assert(false);
}

} // namespace

std::string printAst(ProgramDescription const &program) {
    auto stream = std::stringstream{};

    stream << "(" << std::endl;
    for (const auto &stmt : program.ast) {
        stream << printStatement(stmt, program.functions) << std::endl;
    }
    stream << ")" << std::endl;

    return stream.str();
}
