#include "lexer.h"

#include <cassert>
#include <charconv>
#include <iostream>
#include <optional>

#include "strings.h"

namespace {

std::pair<std::optional<TokenStringLiteral>, std::string_view>
consumeStringLiteral(std::string_view input) {
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

std::pair<std::optional<TokenIntLiteral>, std::string_view>
consumeIntLiteral(std::string_view input) {
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

std::pair<std::optional<TokenIdentifier>, std::string_view>
consumeIdentifier(std::string_view input) {
  assert(!input.empty());

  constexpr auto isIdentifierChar = [](const char c) -> bool {
    return std::isalnum(c) || (c == '_');
  };

  if (!isIdentifierChar(input.front())) {
    std::cerr << "Not a valid character for identifier: " << input.front()
              << std::endl;
    return {{}, input};
  }

  auto len = size_t{0};
  while (len < input.size() && isIdentifierChar(input.at(len))) {
    len += 1;
  }

  assert(len > 0);

  auto token = TokenIdentifier{.name = input.substr(0, len)};
  return {token, input.substr(len)};
}

std::pair<std::optional<Token>, std::string_view>
consumeOneToken(std::string_view input) {
  input = consumeWhitespace(input);
  if (input.empty()) {
    return {{}, input};
  }

  const char front = input.front();

  if (front == '(') {
    return {TokenLBrace{}, input.substr(1)};
  }

  if (front == ')') {
    return {TokenRBrace{}, input.substr(1)};
  }

  if (front == ',') {
    return {TokenComma{}, input.substr(1)};
  }

  if (front == '+') {
    return {TokenPlus{}, input.substr(1)};
  }

  if (front == '-') {
    return {TokenMinus{}, input.substr(1)};
  }

  if (front == ':' && input.size() >= 2) {
    if (input[1] == '=') {
      return {TokenAssignment{}, input.substr(2)};
    }
  }

  if (front == '"') {
    return consumeStringLiteral(input);
  }

  if (std::isdigit(front)) {
    return consumeIntLiteral(input);
  }

  return consumeIdentifier(input);
}

} // namespace

std::vector<Token> lexSourceCode(std::string_view input) {
  auto tokens = std::vector<Token>{};

  while (!input.empty()) {
    auto [token, inputTail] = consumeOneToken(input);

    if (input.size() == inputTail.size()) {
      std::cerr << "Failed to consume any input! Removing one character from "
                   "the input.";
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
