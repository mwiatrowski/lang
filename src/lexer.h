#pragma once

#include <string>
#include <string_view>
#include <variant>
#include <vector>

struct TokenLBrace {};
struct TokenRBrace {};
struct TokenComma {};
struct TokenPlus {};
struct TokenMinus {};
struct TokenAssignment {};
struct TokenIdentifier {
  std::string_view name;
};
struct TokenStringLiteral {
  std::string_view value;
};
struct TokenIntLiteral {
  int64_t value;
};

using Token = std::variant<TokenLBrace, TokenRBrace, TokenComma, TokenPlus,
                           TokenMinus, TokenAssignment, TokenIdentifier,
                           TokenStringLiteral, TokenIntLiteral>;

std::string printToken(const Token &token);
std::string printTokens(const std::vector<Token> &tokens);

std::vector<Token> lexSourceCode(std::string_view input);
