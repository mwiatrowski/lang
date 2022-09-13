#include "lexer.h"

#include <cassert>
#include <charconv>
#include <iostream>
#include <optional>
#include <sstream>
#include <string>

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

  if (front == '"') {
    return consumeStringLiteral(input);
  }

  if (std::isdigit(front)) {
    return consumeIntLiteral(input);
  }

  return consumeIdentifier(input);
}

} // namespace

std::string printToken(const Token &token) {
  if (std::holds_alternative<TokenLBrace>(token)) {
    return "LBRACE";
  } else if (std::holds_alternative<TokenRBrace>(token)) {
    return "RBRACE";
  } else if (std::holds_alternative<TokenComma>(token)) {
    return "COMMA";
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
