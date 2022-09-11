#include "lexer.h"

#include <cassert>
#include <iostream>
#include <sstream>
#include <string>

#include "strings.h"

namespace {

std::pair<std::vector<Token>, std::string_view>
consumeStringLiteral(std::string_view input) {
  assert(!input.empty());
  assert(input.at(0) == '"');

  auto end = input.find_first_of("\"\n", 1);

  if (end == std::string::npos) {
    std::cerr << "Unterminated string literal!" << std::endl;
    return std::make_pair(std::vector<Token>{}, std::string_view{});
  }

  if (input.at(end) == '\n') {
    std::cerr << "Unterminated string literal!" << std::endl;
    input.remove_prefix(end + 1);
    return std::make_pair(std::vector<Token>{}, input);
  }

  auto newToken = TokenStringLiteral{.value = input.substr(1, end - 1)};
  input.remove_prefix(end + 1);
  return std::make_pair(std::vector<Token>{newToken}, input);
}

bool isIdentifierChar(const char c) { return std::isalnum(c) || (c == '_'); }

std::pair<std::vector<Token>, std::string_view>
consumeIdentifier(std::string_view input) {
  assert(!input.empty());

  if (std::isdigit(input.front())) {
    std::cerr << "Identifier can't start with a digit!" << std::endl;
    return std::make_pair(std::vector<Token>{}, input);
  }

  auto len = size_t{0};
  while (len < input.size() && isIdentifierChar(input.at(len))) {
    len += 1;
  }

  if (len == 0) {
    std::cerr << "Not a valid character for identifier: " << input.front()
              << std::endl;
    return {std::vector<Token>{}, input};
  }

  auto token = TokenIdentifier{.name = input.substr(0, len)};
  input.remove_prefix(len);
  return {std::vector<Token>{token}, input};
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
    input = consumeWhitespace(input);
    if (input.empty()) {
      break;
    }

    auto inputSizeBefore = input.size();

    switch (input.front()) {
    case '(': {
      tokens.push_back(TokenLBrace{});
      input.remove_prefix(1);
      break;
    }
    case ')': {
      tokens.push_back(TokenRBrace{});
      input.remove_prefix(1);
      break;
    }
    case ',': {
      tokens.push_back(TokenComma{});
      input.remove_prefix(1);
      break;
    }
    case '"': {
      auto [newTokens, newInput] = consumeStringLiteral(input);
      tokens.insert(tokens.end(), newTokens.begin(), newTokens.end());
      input = newInput;
      break;
    }
    default: {
      // Must be an identifier
      auto [newTokens, newInput] = consumeIdentifier(input);
      tokens.insert(tokens.end(), newTokens.begin(), newTokens.end());
      input = newInput;
      break;
    }
    }

    if (input.size() == inputSizeBefore) {
      assert(!input.empty());
      std::cerr
          << "Failed consume any token! Removing one character from the input."
          << std::endl;
      input.remove_prefix(1);
    }
  }

  return tokens;
}
