#include "ast.h"

#include <cassert>
#include <iostream>
#include <sstream>
#include <string>

#include "variant_helpers.h"

namespace {

std::string printStatement(const AstNodeStmt &stmt, const FuncDefs &functions);

std::string printExpression(const AstNodeExpr &expr, const FuncDefs &functions) {
    if (std::holds_alternative<AstNodeIntLiteral>(expr)) {
        auto literal = std::get<AstNodeIntLiteral>(expr);
        return "(INT_LITERAL " + std::to_string(literal.value.value) + ")";
    } else if (std::holds_alternative<AstNodeStringLiteral>(expr)) {
        auto literal = std::get<AstNodeStringLiteral>(expr);
        return "(STRING_LITERAL " + std::string{literal.value.value} + ")";
    } else if (std::holds_alternative<AstNodeIdentifier>(expr)) {
        auto identifier = std::get<AstNodeIdentifier>(expr);
        return "(IDENTIFIER " + std::string{identifier.value.name} + ")";
    } else if (std::holds_alternative<AstNodeFuncCall>(expr)) {
        auto funcCall = std::get<AstNodeFuncCall>(expr);
        auto stream = std::stringstream{};
        stream << "(CALL " << funcCall.functionName.name;
        for (const auto &arg : funcCall.arguments) {
            stream << " " << printExpression(arg, functions);
        }
        stream << ")";
        return stream.str();
    } else if (auto binaryOp = to<AstNodeBinaryOp>(expr)) {
        assert(binaryOp->operands.size() == 2);
        auto const &lhs = binaryOp->operands[0];
        auto const &rhs = binaryOp->operands[1];
        return "(" + printExpression(lhs, functions) + " " + printToken(binaryOp->op) + " " +
               printExpression(rhs, functions) + ")";
    } else if (std::holds_alternative<AstNodeNegation>(expr)) {
        auto negation = std::get<AstNodeNegation>(expr);
        const auto &operands = negation.operands;
        assert(operands.size() == 1);
        return "( - " + printExpression(operands[0], functions) + ")";
    } else if (const auto funcRef = to<AstNodeFuncRef>(expr)) {
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
    }

    std::cerr << "Unexpected expression type! Index: " << expr.index() << std::endl;
    assert(false);
}

std::string printStatement(const AstNodeStmt &stmt, const FuncDefs &functions) {
    if (std::holds_alternative<AstNodeFuncCall>(stmt)) {
        auto funcCall = std::get<AstNodeFuncCall>(stmt);
        return printExpression(AstNodeExpr{funcCall}, functions);
    }

    if (std::holds_alternative<AstNodeAssignment>(stmt)) {
        auto assignment = std::get<AstNodeAssignment>(stmt);
        return std::string{assignment.variable.name} +
               " := " + printExpression(AstNodeExpr{assignment.value}, functions);
    }

    if (const auto scope = to<AstNodeScope>(stmt)) {
        auto out = std::stringstream{};
        out << "{\n";
        for (const auto &subStmt : scope->statements) {
            out << printStatement(subStmt, functions) << "\n";
        }
        out << "}";
        return out.str();
    }

    std::cerr << "Unexpected statement type! Index: " << stmt.index() << std::endl;
    assert(false);
}

} // namespace

std::string printAst(StmtList const &ast, FuncDefs const &functions) {
    auto stream = std::stringstream{};

    stream << "(" << std::endl;
    for (const auto &stmt : ast) {
        stream << "\t" << printStatement(stmt, functions) << std::endl;
    }
    stream << ")" << std::endl;

    return stream.str();
}
