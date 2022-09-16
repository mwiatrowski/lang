#include "tokens.h"

#include <iostream>
#include <sstream>

std::string printToken(const Token &token) {
  if (std::holds_alternative<TokenLBrace>(token)) {
    return "LBRACE";
  } else if (std::holds_alternative<TokenRBrace>(token)) {
    return "RBRACE";
  } else if (std::holds_alternative<TokenComma>(token)) {
    return "COMMA";
  } else if (std::holds_alternative<TokenPlus>(token)) {
    return "PLUS";
  } else if (std::holds_alternative<TokenMinus>(token)) {
    return "MINUS";
  } else if (std::holds_alternative<TokenAssignment>(token)) {
    return "ASSIGN";
  } else if (std::holds_alternative<TokenIdentifier>(token)) {
    const auto &identifier = std::get<TokenIdentifier>(token);
    return "(IDENTIFIER " + std::string{identifier.name} + ")";
  } else if (std::holds_alternative<TokenStringLiteral>(token)) {
    const auto &stringLiteral = std::get<TokenStringLiteral>(token);
    return "(STRING_LITERAL " + std::string{stringLiteral.value} + ")";
  } else if (std::holds_alternative<TokenIntLiteral>(token)) {
    const auto &intLiteral = std::get<TokenIntLiteral>(token);
    return "(INT_LITERAL " + std::to_string(intLiteral.value) + ")";
  }

  std::cerr << "Unexpected token type! Index: " << token.index() << std::endl;
  return "UNKNOWN_TOKEN_TYPE";
}

std::string printTokens(const std::vector<Token> &tokens) {
  auto stream = std::stringstream{};
  stream << "(" << std::endl;
  for (const auto &token : tokens) {
    stream << "\t" << printToken(token) << std::endl;
  }
  stream << ")" << std::endl;
  return stream.str();
}
