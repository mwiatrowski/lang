#include "lexer.h"

#include <cassert>
#include <charconv>
#include <iostream>
#include <optional>
#include <tuple>

#include "strings.h"

namespace {

std::pair<std::optional<TokenStringLiteral>, std::string_view> consumeStringLiteral(std::string_view input) {
    assert(!input.empty());
    assert(input.at(0) == '"');

    auto end = input.find_first_of("\"\n", 1);

    if (end == std::string::npos) {
        std::cerr << "Unterminated string literal!" << std::endl;
        return {{}, std::string_view{}};
    }

    if (input.at(end) == '\n') {
        std::cerr << "Unterminated string literal!" << std::endl;
        return {{}, input.substr(end + 1)};
    }

    auto newToken = TokenStringLiteral{.value = input.substr(1, end - 1)};
    return {newToken, input.substr(end + 1)};
}

std::pair<std::optional<TokenIntLiteral>, std::string_view> consumeIntLiteral(std::string_view input) {
    assert(!input.empty());
    assert(std::isdigit(input.at(0)));

    auto len = size_t{0};
    while (len < input.size() && std::isdigit(input.at(len))) {
        len += 1;
    }

    assert(len > 0);

    auto value = int64_t{};
    auto numStr = input.substr(0, len);
    auto [matchEnd, error] = std::from_chars(numStr.begin(), numStr.end(), value);

    auto consumedInputLen = std::distance(numStr.begin(), matchEnd);
    auto inputTail = input.substr(consumedInputLen);

    if (error != std::errc{}) {
        std::cerr << "Couldn't parse the integer literal: " << numStr << std::endl;
        return {{}, inputTail};
    }

    auto token = TokenIntLiteral{.value = value};
    return {token, inputTail};
}

std::pair<std::optional<Token>, std::string_view> consumeIdentifierOrKeyword(std::string_view input) {
    assert(!input.empty());

    constexpr auto isIdentifierChar = [](const char c) -> bool { return std::isalnum(c) || (c == '_'); };

    if (!isIdentifierChar(input.front())) {
        std::cerr << "Not a valid character for identifier: " << input.front() << std::endl;
        return {{}, input};
    }

    auto len = size_t{0};
    while (len < input.size() && isIdentifierChar(input.at(len))) {
        len += 1;
    }

    assert(len > 0);

    auto tokenVal = std::string_view{};
    std::tie(tokenVal, input) = cutStr(input, len);

    if (tokenVal == "fn") {
        return {TokenKwFn{}, input};
    }

    if (tokenVal == "if") {
        return {TokenKwIf{}, input};
    }

    if (tokenVal == "elif") {
        return {TokenKwElif{}, input};
    }

    if (tokenVal == "else") {
        return {TokenKwElse{}, input};
    }

    if (tokenVal == "while") {
        return {TokenKwWhile{}, input};
    }

    if (tokenVal == "break") {
        return {TokenKwBreak{}, input};
    }

    if (tokenVal == "continue") {
        return {TokenKwContinue{}, input};
    }

    if (tokenVal == "true") {
        return {TokenBoolLiteral{true}, input};
    }

    if (tokenVal == "false") {
        return {TokenBoolLiteral{false}, input};
    }

    return {TokenIdentifier{.name = tokenVal}, input};
}

std::string_view consumeWhitespaceAndComments(std::string_view input) {
    while (!input.empty()) {
        auto inputSizeBefore = input.size();

        if (input.at(0) == '#') {
            input = fastForwardTo(input, '\n');
        }
        input = consumeWhitespace(input);

        if (input.size() == inputSizeBefore) {
            break;
        }
    }

    return input;
}

std::pair<std::optional<Token>, std::string_view> consumeOneToken(std::string_view input) {
    input = consumeWhitespaceAndComments(input);
    if (input.empty()) {
        return {{}, input};
    }

    const char front = input.front();

    if (front == '#') {
    }

    if (front == '(') {
        return {TokenLBrace{}, input.substr(1)};
    }

    if (front == ')') {
        return {TokenRBrace{}, input.substr(1)};
    }

    if (front == '{') {
        return {TokenLCurBrace{}, input.substr(1)};
    }

    if (front == '}') {
        return {TokenRCurBrace{}, input.substr(1)};
    }

    if (front == ',') {
        return {TokenComma{}, input.substr(1)};
    }

    if (front == '+') {
        return {TokenPlus{}, input.substr(1)};
    }

    if (input.size() >= 2 && input.starts_with("->")) {
        return {TokenRArrow{}, input.substr(2)};
    }

    if (front == '-') {
        return {TokenMinus{}, input.substr(1)};
    }

    if (input.starts_with("<=")) {
        return {TokenLessOrEqual{}, input.substr(2)};
    }

    if (input.starts_with('<')) {
        return {TokenLess{}, input.substr(1)};
    }

    if (input.starts_with(">=")) {
        return {TokenGreaterOrEqual{}, input.substr(2)};
    }

    if (input.starts_with('>')) {
        return {TokenGreater{}, input.substr(1)};
    }

    if (input.starts_with("=")) {
        return {TokenEqual{}, input.substr(1)};
    }

    if (input.starts_with("!=")) {
        return {TokenNotEqual{}, input.substr(2)};
    }

    if (input.size() >= 2 && input.starts_with(":=")) {
        return {TokenAssignment{}, input.substr(2)};
    }

    if (front == ':') {
        return {TokenColon{}, input.substr(1)};
    }

    if (front == '"') {
        return consumeStringLiteral(input);
    }

    if (std::isdigit(front)) {
        return consumeIntLiteral(input);
    }

    return consumeIdentifierOrKeyword(input);
}

} // namespace

std::vector<Token> lexSourceCode(std::string_view input) {
    auto tokens = std::vector<Token>{};

    while (!input.empty()) {
        auto [token, inputTail] = consumeOneToken(input);

        if (input.size() == inputTail.size()) {
            std::cerr << "Failed to consume any input! Removing one character from the input." << std::endl;
            input.remove_prefix(1);
        } else {
            input = inputTail;
        }

        if (token.has_value()) {
            tokens.push_back(*token);
        }
    }

    return tokens;
}
