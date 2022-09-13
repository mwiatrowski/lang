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

std::pair<std::optional<AstNodeBasicExpr>, TokensSpan>
consumeBasicExpression(TokensSpan tokens);

std::pair<std::optional<AstNodeFuncCall>, TokensSpan>
consumeFunctionCall(TokensSpan tokens) {
  assert((peek<TokenIdentifier, TokenLBrace>(tokens)));

  auto funcName = TokenIdentifier{};
  std::tie(funcName, tokens) = consume<TokenIdentifier>(tokens);
  std::tie(std::ignore, tokens) = consume<TokenLBrace>(tokens);

  if (peek<TokenRBrace>(tokens)) {
    std::tie(std::ignore, tokens) = consume<TokenRBrace>(tokens);
    auto funcCall = AstNodeFuncCall{.functionName = funcName, .arguments = {}};
    return {funcCall, tokens};
  }

  auto args = std::vector<AstNodeBasicExpr>{};

  while (!tokens.empty()) {
    auto expr = std::optional<AstNodeBasicExpr>{};
    std::tie(expr, tokens) = consumeBasicExpression(tokens);

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

std::pair<std::optional<AstNodeBasicExpr>, TokensSpan>
consumeBasicExpression(TokensSpan tokens) {
  assert(!tokens.empty());

  if (peek<TokenIdentifier, TokenLBrace>(tokens)) {
    auto [funcCall, tokensTail] = consumeFunctionCall(tokens);
    auto basicExpr = funcCall.has_value()
                         ? std::optional<AstNodeBasicExpr>{*funcCall}
                         : std::optional<AstNodeBasicExpr>{};
    return {basicExpr, tokensTail};
  }

  if (peek<TokenIdentifier>(tokens)) {
    auto [tokenIdentifier, tokensTail] = consume<TokenIdentifier>(tokens);
    auto identifier = AstNodeIdentifier{.value = tokenIdentifier};
    return {AstNodeBasicExpr{identifier}, tokensTail};
  }

  if (peek<TokenIntLiteral>(tokens)) {
    auto [tokenLiteral, tokensTail] = consume<TokenIntLiteral>(tokens);
    auto literal = AstNodeIntLiteral{.value = tokenLiteral};
    return {AstNodeBasicExpr{literal}, tokensTail};
  }

  if (peek<TokenStringLiteral>(tokens)) {
    auto [tokenLiteral, tokensTail] = consume<TokenStringLiteral>(tokens);
    auto literal = AstNodeStringLiteral{.value = tokenLiteral};
    return {AstNodeBasicExpr{literal}, tokensTail};
  }

  std::cerr << "Failed to parse any expression" << std::endl;
  return {{}, tokens};
}

} // namespace

std::string printBasicExpression(const AstNodeBasicExpr &expr) {
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
      stream << " " << printBasicExpression(arg);
    }
    stream << ")";
    return stream.str();
  }

  std::cerr << "Unexpected expression type! Index: " << expr.index()
            << std::endl;
  return "UNKNOWN_EXPRESSION_TYPE";
}

std::string printAst(const Ast &ast) {
  auto stream = std::stringstream{};

  stream << "(" << std::endl;
  for (const auto &expr : ast) {
    stream << "\t" << printBasicExpression(expr) << std::endl;
  }
  stream << ")" << std::endl;

  return stream.str();
}

Ast parseSourceFile(TokensSpan tokens) {
  auto ast = Ast{};

  while (!tokens.empty()) {
    auto [expr, tokensTail] = consumeBasicExpression(tokens);

    if (tokens.size() == tokensTail.size()) {
      std::cerr << "Failed to consume any input! Skipping this token: "
                << printToken(tokens.front()) << std::endl;
      tokens = tokens.subspan(1);
    } else {
      tokens = tokensTail;
    }

    if (expr.has_value()) {
      ast.push_back(*expr);
    }
  }

  return ast;
}
