#include "parser.h"

#include <cassert>
#include <iostream>
#include <optional>
#include <sstream>
#include <tuple>

namespace {

template <typename... TokenTypes> bool peek(TokensSpan tokens) {
  static_assert(sizeof...(TokenTypes) > 0);

  if (tokens.size() < sizeof...(TokenTypes)) {
    return false;
  }

  auto nextIdx = [idx = size_t{0}]() mutable { return idx++; };
  return (std::holds_alternative<TokenTypes>(tokens[nextIdx()]) && ...);
}

template <typename TokenType>
std::pair<TokenType, TokensSpan> consume(TokensSpan tokens) {
  assert(!tokens.empty());
  const auto &firstToken = tokens[0];

  assert(std::holds_alternative<TokenType>(firstToken));
  const auto &tokenOfSpecificType = std::get<TokenType>(firstToken);

  return {tokenOfSpecificType, tokens.subspan(1)};
}

template <typename TokenType> TokensSpan consumeUntil(TokensSpan tokens) {
  auto lastConsumed =
      std::find_if(tokens.begin(), tokens.end(), [](const Token &token) {
        return std::holds_alternative<TokenType>(token);
      });
  if (lastConsumed == tokens.end()) {
    return {};
  }
  return TokensSpan{std::next(lastConsumed), tokens.end()};
}

std::pair<std::optional<AstNodeExpr>, TokensSpan>
consumeExpression(TokensSpan tokens);

std::pair<std::optional<AstNodeFuncCall>, TokensSpan>
consumeFunctionCall(TokensSpan tokens) {
  if (!peek<TokenIdentifier, TokenLBrace>(tokens)) {
    std::cerr << "Function call must start with the name of the function and "
                 "an opening brace."
              << std::endl;
    return {{}, tokens};
  }

  auto funcName = TokenIdentifier{};
  std::tie(funcName, tokens) = consume<TokenIdentifier>(tokens);
  std::tie(std::ignore, tokens) = consume<TokenLBrace>(tokens);

  if (peek<TokenRBrace>(tokens)) {
    std::tie(std::ignore, tokens) = consume<TokenRBrace>(tokens);
    auto funcCall = AstNodeFuncCall{.functionName = funcName, .arguments = {}};
    return {funcCall, tokens};
  }

  auto args = std::vector<AstNodeExpr>{};

  while (!tokens.empty()) {
    auto expr = std::optional<AstNodeExpr>{};
    std::tie(expr, tokens) = consumeExpression(tokens);

    if (!expr.has_value()) {
      std::cerr << "Expected an expression!" << std::endl;
      return {{}, consumeUntil<TokenRBrace>(tokens)};
    }
    args.push_back(*expr);

    if (tokens.empty()) {
      std::cerr << "No more tokens left!" << std::endl;
      return {{}, {}};
    }

    if (peek<TokenComma>(tokens)) {
      std::tie(std::ignore, tokens) = consume<TokenComma>(tokens);
      continue;
    }

    if (peek<TokenRBrace>(tokens)) {
      std::tie(std::ignore, tokens) = consume<TokenRBrace>(tokens);
      auto funcCall = AstNodeFuncCall{.functionName = funcName,
                                      .arguments = std::move(args)};
      return {funcCall, tokens};
    }

    assert(!tokens.empty());
    std::cerr << "Expected either a comma or a closing brace, got "
              << printToken(tokens.front()) << std::endl;
    return {{}, consumeUntil<TokenRBrace>(tokens)};
  }

  assert(tokens.empty());
  std::cerr << "Expected more tokens!" << std::endl;
  return {{}, {}};
}

std::pair<std::optional<AstNodeExpr>, TokensSpan>
consumeBasicExpression(TokensSpan tokens) {
  if (peek<TokenIdentifier, TokenLBrace>(tokens)) {
    auto [funcCall, tokensTail] = consumeFunctionCall(tokens);
    auto expr = funcCall.has_value() ? std::optional<AstNodeExpr>{*funcCall}
                                     : std::optional<AstNodeExpr>{};
    return {expr, tokensTail};
  }

  if (peek<TokenIdentifier>(tokens)) {
    auto [tokenIdentifier, tokensTail] = consume<TokenIdentifier>(tokens);
    auto identifier = AstNodeIdentifier{.value = tokenIdentifier};
    return {AstNodeExpr{identifier}, tokensTail};
  }

  if (peek<TokenIntLiteral>(tokens)) {
    auto [tokenLiteral, tokensTail] = consume<TokenIntLiteral>(tokens);
    auto literal = AstNodeIntLiteral{.value = tokenLiteral};
    return {AstNodeExpr{literal}, tokensTail};
  }

  if (peek<TokenStringLiteral>(tokens)) {
    auto [tokenLiteral, tokensTail] = consume<TokenStringLiteral>(tokens);
    auto literal = AstNodeStringLiteral{.value = tokenLiteral};
    return {AstNodeExpr{literal}, tokensTail};
  }

  if (peek<TokenMinus>(tokens)) {
    std::tie(std::ignore, tokens) = consume<TokenMinus>(tokens);
    auto expr = std::optional<AstNodeExpr>{};
    std::tie(expr, tokens) = consumeExpression(tokens);

    if (!expr) {
      std::cerr << "Expected an expression after a unary minus" << std::endl;
      return {{}, tokens};
    }

    auto negation = AstNodeNegation{.operands = {*expr}};
    return {AstNodeExpr{std::move(negation)}, tokens};
  }

  std::cerr << "Failed to parse any expression" << std::endl;
  return {{}, tokens};
}

std::pair<std::optional<AstNodeExpr>, TokensSpan>
consumeExpression(TokensSpan tokens) {
  auto firstBasicExpr = std::optional<AstNodeExpr>{};
  std::tie(firstBasicExpr, tokens) = consumeBasicExpression(tokens);

  if (!firstBasicExpr) {
    std::cerr << "Failed to parse an expression!" << std::endl;
    return {{}, tokens};
  }

  auto subExprs = std::vector<AstNodeExpr>{*firstBasicExpr};
  auto operators = std::vector<Token>{};

  while (!tokens.empty()) {
    if (peek<TokenPlus>(tokens)) {
      std::tie(std::ignore, tokens) = consume<TokenPlus>(tokens);
      operators.push_back(TokenPlus{});
    } else if (peek<TokenMinus>(tokens)) {
      std::tie(std::ignore, tokens) = consume<TokenMinus>(tokens);
      operators.push_back(TokenMinus{});
    } else {
      break;
    }

    auto nextBasicExpr = std::optional<AstNodeExpr>{};
    std::tie(nextBasicExpr, tokens) = consumeBasicExpression(tokens);

    if (!nextBasicExpr) {
      std::cerr << "Failed to parse an expression!" << std::endl;
      return {{}, tokens};
    }

    subExprs.push_back(std::move(*nextBasicExpr));
  }

  assert(!subExprs.empty());
  assert(subExprs.size() == operators.size() + 1);

  // Addition and substraction have the same priority, so the resulting tree has
  // a trivial structure.
  auto lhs = std::move(subExprs.front());
  for (auto i = size_t{}; i < operators.size(); ++i) {
    auto rhs = std::move(subExprs.at(i + 1));
    auto op = operators.at(i);

    auto operands = std::vector<AstNodeExpr>{std::move(lhs), std::move(rhs)};
    if (std::holds_alternative<TokenPlus>(op)) {
      auto newLhs = AstNodeAddition{.operands = std::move(operands)};
      lhs = AstNodeExpr{std::move(newLhs)};
    } else if (std::holds_alternative<TokenMinus>(op)) {
      auto newLhs = AstNodeSubstraction{.operands = std::move(operands)};
      lhs = AstNodeExpr{std::move(newLhs)};
    } else {
      std::cerr << "Expected a binary operation, got " << printToken(op)
                << std::endl;
      assert(false);
    }
  }
  return std::make_pair(std::move(lhs), tokens);
}

std::pair<std::optional<AstNodeStmt>, TokensSpan>
consumeAssignment(TokensSpan tokens) {
  if (!peek<TokenIdentifier, TokenAssignment>(tokens)) {
    std::cerr << "Assignment must start with a variable name and an assignment "
                 "operator."
              << std::endl;
    return {{}, tokens};
  }

  auto variable = std::optional<TokenIdentifier>{};
  std::tie(variable, tokens) = consume<TokenIdentifier>(tokens);
  assert(variable.has_value());
  std::tie(std::ignore, tokens) = consume<TokenAssignment>(tokens);

  auto expr = std::optional<AstNodeExpr>{};
  std::tie(expr, tokens) = consumeExpression(tokens);

  if (!expr) {
    std::cerr << "Expected an expression" << std::endl;
    return {{}, tokens};
  }

  auto assignment = AstNodeAssignment{.variable = std::move(*variable),
                                      .value = std::move(*expr)};
  return {std::move(assignment), tokens};
}

std::pair<std::optional<AstNodeStmt>, TokensSpan>
consumeStatement(TokensSpan tokens) {
  if (peek<TokenIdentifier, TokenAssignment>(tokens)) {
    return consumeAssignment(tokens);
  } else if (peek<TokenIdentifier, TokenLBrace>(tokens)) {
    return consumeFunctionCall(tokens);
  }

  std::cerr << "Failed to parse a statement" << std::endl;
  return {{}, tokens};
}

} // namespace

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

Ast parseSourceFile(TokensSpan tokens) {
  auto ast = Ast{};

  while (!tokens.empty()) {
    auto [stmt, tokensTail] = consumeStatement(tokens);

    if (tokens.size() == tokensTail.size()) {
      std::cerr << "Failed to consume any input! Skipping this token: "
                << printToken(tokens.front()) << std::endl;
      tokens = tokens.subspan(1);
    } else {
      tokens = tokensTail;
    }

    if (stmt.has_value()) {
      ast.push_back(*stmt);
    }
  }

  return ast;
}
