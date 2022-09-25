#include "tokens.h"

#include <iostream>
#include <sstream>

std::string printToken(const Token &token) {
    if (std::holds_alternative<TokenLBrace>(token)) {
        return "L_BRACE";
    } else if (std::holds_alternative<TokenRBrace>(token)) {
        return "R_BRACE";
    } else if (std::holds_alternative<TokenLCurBrace>(token)) {
        return "L_CURLY_BRACE";
    } else if (std::holds_alternative<TokenRCurBrace>(token)) {
        return "R_CURLY_BRACE";
    } else if (std::holds_alternative<TokenComma>(token)) {
        return "COMMA";
    } else if (std::holds_alternative<TokenColon>(token)) {
        return "COLON";
    } else if (std::holds_alternative<TokenRArrow>(token)) {
        return "R_ARROW";
    } else if (std::holds_alternative<TokenPlus>(token)) {
        return "PLUS";
    } else if (std::holds_alternative<TokenMinus>(token)) {
        return "MINUS";
    } else if (std::holds_alternative<TokenLess>(token)) {
        return "LESS";
    } else if (std::holds_alternative<TokenLessOrEqual>(token)) {
        return "LESS_OR_EQUAL";
    } else if (std::holds_alternative<TokenGreater>(token)) {
        return "GREATER";
    } else if (std::holds_alternative<TokenGreaterOrEqual>(token)) {
        return "GREATER_OR_EQUAL";
    } else if (std::holds_alternative<TokenEqual>(token)) {
        return "EQUAL";
    } else if (std::holds_alternative<TokenNotEqual>(token)) {
        return "NOT_EQUAL";
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
    } else if (std::holds_alternative<TokenKwFn>(token)) {
        return "FN";
    } else if (std::holds_alternative<TokenKwIf>(token)) {
        return "IF";
    } else if (std::holds_alternative<TokenKwElif>(token)) {
        return "ELIF";
    } else if (std::holds_alternative<TokenKwElse>(token)) {
        return "ELSE";
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
