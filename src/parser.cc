#include "parser.h"

#include <cassert>
#include <iostream>
#include <optional>
#include <sstream>

namespace {

template <typename TokenType>
std::pair<std::optional<TokenType>, TokensSpan> consume(TokensSpan tokens) {
  if (tokens.empty()) {
    return {{}, tokens};
  }

  const auto &firstToken = tokens.front();
  if (!std::holds_alternative<TokenType>(firstToken)) {
    return {{}, tokens};
  }

  const auto &specificToken = std::get<TokenType>(firstToken);
  return {specificToken, tokens.subspan(1)};
}

std::optional<std::pair<AstNodeFuncCall, TokensSpan>>
consumeFunctionCall(TokensSpan tokens) {
  assert(!tokens.empty());

  auto [funcName, tAfterName] = consume<TokenIdentifier>(tokens);
  if (!funcName) {
    std::cerr << "Expected an identifier!" << std::endl;
    return {};
  }

  auto [lBrace, tAfterLBrace] = consume<TokenLBrace>(tAfterName);
  if (!lBrace) {
    std::cerr << "Expected an opening brace!" << std::endl;
    return {};
  }

  if (tokens.empty()) {
    std::cerr << "Expected more tokens!" << std::endl;
    return {};
  }

  {
    const auto &next = tAfterLBrace.front();
    if (std::holds_alternative<TokenRBrace>(next)) {
      auto [rBrace, tAfterRBrace] = consume<TokenRBrace>(tAfterLBrace);
      auto funcCall =
          AstNodeFuncCall{.functionName = *funcName, .arguments = {}};
      return {{funcCall, tAfterRBrace}};
    }
  }

  auto args = std::vector<TokenStringLiteral>{};
  auto tokensTail = tAfterLBrace;
  do {
    auto [arg, tAfterArg] = consume<TokenStringLiteral>(tokensTail);
    if (!arg) {
      std::cerr << "Expected an argument!" << std::endl;
      return {};
    }

    args.push_back(*arg);

    if (tokens.empty()) {
      std::cerr << "Expected a comma or a closing brace!" << std::endl;
      return {};
    }

    auto next = tAfterArg.front();

    if (std::holds_alternative<TokenRBrace>(next)) {
      auto [rBrace, tAfterRBrace] = consume<TokenRBrace>(tAfterArg);
      auto funcCall = AstNodeFuncCall{.functionName = *funcName,
                                      .arguments = std::move(args)};
      return {{std::move(funcCall), tAfterRBrace}};
    }

    if (std::holds_alternative<TokenComma>(next)) {
      auto [comma, tAfterComma] = consume<TokenComma>(tAfterArg);
      tokensTail = tAfterComma;
    } else {
      std::cerr << "Expected a comma or a closing brace!" << std::endl;
      return {};
    }
  } while (!tokens.empty());

  std::cerr << "Expected more tokens!" << std::endl;
  return {};
}

} // namespace

std::string printAst(const Ast &ast) {
  auto stream = std::stringstream{};

  stream << "(" << std::endl;
  for (const auto &node : ast) {
    const auto &funcCall = std::get<AstNodeFuncCall>(node);
    stream << "\t(CALL " << funcCall.functionName.name;
    for (const auto &arg : funcCall.arguments) {
      stream << " " << arg.value;
    }
    stream << ")" << std::endl;
  }
  stream << ")" << std::endl;

  return stream.str();
}

Ast parseSourceFile(TokensSpan tokens) {
  auto ast = Ast{};

  while (!tokens.empty()) {
    if (const auto parseResult = consumeFunctionCall(tokens)) {
      const auto &[funcCall, tokensTail] = *parseResult;
      ast.push_back(funcCall);
      tokens = tokensTail;
      continue;
    }

    std::cerr << "Parser error! Skipping to the next token." << std::endl;
    tokens = tokens.subspan(1);
  }

  return ast;
}
