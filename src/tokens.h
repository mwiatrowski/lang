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
struct TokenDot {};

struct TokenPlus {};
struct TokenMinus {};

struct TokenLess {};
struct TokenLessOrEqual {};
struct TokenGreater {};
struct TokenGreaterOrEqual {};
struct TokenEqual {};
struct TokenNotEqual {};

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
struct TokenBoolLiteral {
    bool value;
};

struct TokenKwFn {};

struct TokenKwStruct {};

struct TokenKwIf {};
struct TokenKwElif {};
struct TokenKwElse {};

struct TokenKwWhile {};
struct TokenKwBreak {};
struct TokenKwContinue {};

using Token = std::variant<TokenLBrace, TokenRBrace, TokenLCurBrace, TokenRCurBrace, TokenComma, TokenColon,
                           TokenRArrow, TokenDot, TokenPlus, TokenMinus, TokenLess, TokenLessOrEqual, TokenGreater,
                           TokenGreaterOrEqual, TokenEqual, TokenNotEqual, TokenAssignment, TokenIdentifier,
                           TokenStringLiteral, TokenIntLiteral, TokenBoolLiteral, TokenKwFn, TokenKwStruct, TokenKwIf,
                           TokenKwElif, TokenKwElse, TokenKwWhile, TokenKwBreak, TokenKwContinue>;

std::string printToken(const Token &token);
std::string printTokens(const std::vector<Token> &tokens);
