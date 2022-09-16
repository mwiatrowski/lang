#include "ast.h"

#include <cassert>
#include <iostream>
#include <sstream>
#include <string>

std::string printExpression(const AstNodeExpr &expr) {
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
      stream << " " << printExpression(arg);
    }
    stream << ")";
    return stream.str();
  } else if (std::holds_alternative<AstNodeAddition>(expr)) {
    auto addition = std::get<AstNodeAddition>(expr);
    const auto &operands = addition.operands;
    assert(operands.size() == 2);
    return "(" + printExpression(operands[0]) + " + " +
           printExpression(operands[1]) + ")";
  } else if (std::holds_alternative<AstNodeSubstraction>(expr)) {
    auto substraction = std::get<AstNodeSubstraction>(expr);
    const auto &operands = substraction.operands;
    assert(operands.size() == 2);
    return "(" + printExpression(operands[0]) + " - " +
           printExpression(operands[1]) + ")";
  } else if (std::holds_alternative<AstNodeNegation>(expr)) {
    auto negation = std::get<AstNodeNegation>(expr);
    const auto &operands = negation.operands;
    assert(operands.size() == 1);
    return "( - " + printExpression(operands[0]) + ")";
  }

  std::cerr << "Unexpected expression type! Index: " << expr.index()
            << std::endl;
  assert(false);
}

std::string printStatement(const AstNodeStmt &stmt) {
  if (std::holds_alternative<AstNodeFuncCall>(stmt)) {
    auto funcCall = std::get<AstNodeFuncCall>(stmt);
    return printExpression(AstNodeExpr{funcCall});
  } else if (std::holds_alternative<AstNodeAssignment>(stmt)) {
    auto assignment = std::get<AstNodeAssignment>(stmt);
    return std::string{assignment.variable.name} +
           " := " + printExpression(AstNodeExpr{assignment.value});
  }

  std::cerr << "Unexpected statement type! Index: " << stmt.index()
            << std::endl;
  assert(false);
}

std::string printAst(const Ast &ast) {
  auto stream = std::stringstream{};

  stream << "(" << std::endl;
  for (const auto &stmt : ast) {
    stream << "\t" << printStatement(stmt) << std::endl;
  }
  stream << ")" << std::endl;

  return stream.str();
}
