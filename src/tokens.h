#pragma once

#include <string>
#include <string_view>
#include <variant>
#include <vector>

struct TokenLBrace {};
struct TokenRBrace {};
struct TokenLCurBrace {};
struct TokenRCurBrace {};

struct TokenComma {};
struct TokenColon {};
struct TokenRArrow {};

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

struct TokenKwFn {};

using Token =
    std::variant<TokenLBrace, TokenRBrace, TokenLCurBrace, TokenRCurBrace,
                 TokenComma, TokenColon, TokenRArrow, TokenPlus, TokenMinus,
                 TokenAssignment, TokenIdentifier, TokenStringLiteral,
                 TokenIntLiteral, TokenKwFn>;

std::string printToken(const Token &token);
std::string printTokens(const std::vector<Token> &tokens);
