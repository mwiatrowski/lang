#include "parser.h"

#include <cassert>
#include <iostream>
#include <optional>
#include <tuple>

namespace {

template <typename... TokenTypes> bool peek(TokensSpan tokens) {
    static_assert(sizeof...(TokenTypes) > 0);

    if (tokens.size() < sizeof...(TokenTypes)) {
        return false;
    }

    auto nextIdx = [idx = size_t{0}]() mutable { return idx++; };
    return (std::holds_alternative<TokenTypes>(tokens[nextIdx()]) && ...);
}

template <typename TokenType> std::pair<TokenType, TokensSpan> consume(TokensSpan tokens) {
    assert(!tokens.empty());
    const auto &firstToken = tokens[0];

    assert(std::holds_alternative<TokenType>(firstToken));
    const auto &tokenOfSpecificType = std::get<TokenType>(firstToken);

    return {tokenOfSpecificType, tokens.subspan(1)};
}

template <typename TokenType> [[nodiscard]] TokensSpan consumeUntil(TokensSpan tokens) {
    auto lastConsumed = std::find_if(tokens.begin(), tokens.end(),
                                     [](const Token &token) { return std::holds_alternative<TokenType>(token); });
    if (lastConsumed == tokens.end()) {
        return {};
    }
    return TokensSpan{std::next(lastConsumed), tokens.end()};
}

struct ParserContext {
    TokensSpan tokens;
    Ast ast;
};

using TypedArgList = decltype(AstNodeFuncDef::arguments);

std::optional<AstNodeExpr> consumeExpression(ParserContext &ctx);
std::optional<AstNodeStmt> consumeStatement(ParserContext &ctx);

std::optional<TypedArgList> consumeTypedArgList(ParserContext &ctx) {
    auto &tokens = ctx.tokens;

    if (!peek<TokenLBrace>(tokens)) {
        std::cerr << "Expected an opening brace." << std::endl;
        return {};
    }
    std::tie(std::ignore, tokens) = consume<TokenLBrace>(tokens);

    if (peek<TokenRBrace>(tokens)) {
        std::tie(std::ignore, tokens) = consume<TokenRBrace>(tokens);
        return TypedArgList{};
    }

    auto args = TypedArgList{};
    while (!tokens.empty()) {
        if (!peek<TokenIdentifier, TokenColon, TokenIdentifier>(tokens)) {
            std::cerr << "Expected the name of the argument and its type." << std::endl;
            tokens = consumeUntil<TokenRBrace>(tokens);
            return {};
        }

        auto argName = TokenIdentifier{};
        auto argType = TokenIdentifier{};
        std::tie(argName, tokens) = consume<TokenIdentifier>(tokens);
        std::tie(std::ignore, tokens) = consume<TokenColon>(tokens);
        std::tie(argType, tokens) = consume<TokenIdentifier>(tokens);

        args.emplace_back(std::move(argName), std::move(argType));

        if (peek<TokenComma>(tokens)) {
            std::tie(std::ignore, tokens) = consume<TokenComma>(tokens);
            continue;
        }

        if (peek<TokenRBrace>(tokens)) {
            std::tie(std::ignore, tokens) = consume<TokenRBrace>(tokens);
            return args;
        }

        std::cerr << "Expected either a comma or a closing brace!" << std::endl;
        tokens = consumeUntil<TokenRBrace>(tokens);
        return {};
    }

    assert(tokens.empty());
    std::cerr << "Expected more tokens!" << std::endl;
    return {};
}

std::optional<AstNodeFuncDef> consumeFunctionDefinition(ParserContext &ctx) {
    auto &tokens = ctx.tokens;

    if (!peek<TokenKwFn>(tokens)) {
        std::cerr << "Expected a function definition." << std::endl;
        return {};
    }
    std::tie(std::ignore, tokens) = consume<TokenKwFn>(tokens);

    auto funcArgs = consumeTypedArgList(ctx);
    if (!funcArgs) {
        std::cerr << "Expected a list of arguments." << std::endl;
        return {};
    }

    if (!peek<TokenRArrow>(tokens)) {
        std::cerr << "Expected a right arrow." << std::endl;
        return {};
    }
    std::tie(std::ignore, tokens) = consume<TokenRArrow>(tokens);

    auto funcRetVals = consumeTypedArgList(ctx);
    if (!funcRetVals) {
        std::cerr << "Expected a list of return values." << std::endl;
        return {};
    }

    if (!peek<TokenLCurBrace>(tokens)) {
        std::cerr << "Expected a curly opening brace." << std::endl;
        return {};
    }
    std::tie(std::ignore, tokens) = consume<TokenLCurBrace>(tokens);

    auto funcBody = std::vector<AstNodeStmt>{};
    do {
        if (peek<TokenRCurBrace>(tokens)) {
            std::tie(std::ignore, tokens) = consume<TokenRCurBrace>(tokens);

            return AstNodeFuncDef{.arguments = std::move(*funcArgs),
                                  .returnVals = std::move(*funcRetVals),
                                  .functionBody = std::move(funcBody)};
        }

        auto stmt = consumeStatement(ctx);
        if (!stmt) {
            std::cerr << "Expected a statement." << std::endl;
            tokens = consumeUntil<TokenRCurBrace>(tokens);
            return {};
        }

        funcBody.push_back(std::move(*stmt));
    } while (!tokens.empty());

    std::cerr << "Expected more tokens!" << std::endl;
    return {};
}

std::optional<AstNodeFuncCall> consumeFunctionCall(ParserContext &ctx) {
    auto &tokens = ctx.tokens;

    if (!peek<TokenIdentifier, TokenLBrace>(tokens)) {
        std::cerr << "Function call must start with the name of the function and an opening brace." << std::endl;
        return {};
    }

    auto funcName = TokenIdentifier{};
    std::tie(funcName, tokens) = consume<TokenIdentifier>(tokens);
    std::tie(std::ignore, tokens) = consume<TokenLBrace>(tokens);

    if (peek<TokenRBrace>(tokens)) {
        std::tie(std::ignore, tokens) = consume<TokenRBrace>(tokens);
        return AstNodeFuncCall{.functionName = funcName, .arguments = {}};
    }

    auto args = std::vector<AstNodeExpr>{};

    while (!tokens.empty()) {
        auto expr = consumeExpression(ctx);

        if (!expr.has_value()) {
            std::cerr << "Expected an expression!" << std::endl;
            tokens = consumeUntil<TokenRBrace>(tokens);
            return {};
        }
        args.push_back(*expr);

        if (tokens.empty()) {
            std::cerr << "No more tokens left!" << std::endl;
            return {};
        }

        if (peek<TokenComma>(tokens)) {
            std::tie(std::ignore, tokens) = consume<TokenComma>(tokens);
            continue;
        }

        if (peek<TokenRBrace>(tokens)) {
            std::tie(std::ignore, tokens) = consume<TokenRBrace>(tokens);
            return AstNodeFuncCall{.functionName = funcName, .arguments = std::move(args)};
        }

        assert(!tokens.empty());
        std::cerr << "Expected either a comma or a closing brace, got " << printToken(tokens.front()) << std::endl;
        tokens = consumeUntil<TokenRBrace>(tokens);
        return {};
    }

    assert(tokens.empty());
    std::cerr << "Expected more tokens!" << std::endl;
    return {};
}

std::optional<AstNodeExpr> consumeBasicExpression(ParserContext &ctx) {
    auto &tokens = ctx.tokens;

    if (peek<TokenIdentifier, TokenLBrace>(tokens)) {
        auto funcCall = consumeFunctionCall(ctx);
        return funcCall.has_value() ? std::optional<AstNodeExpr>{*funcCall} : std::optional<AstNodeExpr>{};
    }

    if (peek<TokenIdentifier>(tokens)) {
        auto [tokenIdentifier, tokensTail] = consume<TokenIdentifier>(tokens);
        tokens = tokensTail;
        return AstNodeExpr{AstNodeIdentifier{.value = tokenIdentifier}};
    }

    if (peek<TokenIntLiteral>(tokens)) {
        auto [tokenLiteral, tokensTail] = consume<TokenIntLiteral>(tokens);
        tokens = tokensTail;
        return AstNodeExpr{AstNodeIntLiteral{.value = tokenLiteral}};
    }

    if (peek<TokenStringLiteral>(tokens)) {
        auto [tokenLiteral, tokensTail] = consume<TokenStringLiteral>(tokens);
        tokens = tokensTail;
        return AstNodeExpr{AstNodeStringLiteral{.value = tokenLiteral}};
    }

    if (peek<TokenMinus>(tokens)) {
        std::tie(std::ignore, tokens) = consume<TokenMinus>(tokens);
        auto expr = consumeExpression(ctx);

        if (!expr) {
            std::cerr << "Expected an expression after a unary minus" << std::endl;
            return {};
        }

        return AstNodeExpr{AstNodeNegation{.operands = {*expr}}};
    }

    if (peek<TokenKwFn>(tokens)) {
        auto funcDef = consumeFunctionDefinition(ctx);
        return funcDef.has_value() ? std::optional<AstNodeExpr>{*funcDef} : std::optional<AstNodeExpr>{};
    }

    std::cerr << "Failed to parse any expression" << std::endl;
    return {};
}

std::optional<AstNodeExpr> consumeExpression(ParserContext &ctx) {
    auto &tokens = ctx.tokens;

    auto firstBasicExpr = consumeBasicExpression(ctx);
    if (!firstBasicExpr) {
        std::cerr << "Failed to parse an expression!" << std::endl;
        return {};
    }

    auto subExprs = std::vector<AstNodeExpr>{*firstBasicExpr};
    auto operators = std::vector<Token>{};

    while (!tokens.empty()) {
        if (peek<TokenPlus>(tokens)) {
            std::tie(std::ignore, tokens) = consume<TokenPlus>(tokens);
            operators.push_back(TokenPlus{});
        } else if (peek<TokenMinus>(tokens)) {
            std::tie(std::ignore, tokens) = consume<TokenMinus>(tokens);
            operators.push_back(TokenMinus{});
        } else {
            break;
        }

        auto nextBasicExpr = consumeBasicExpression(ctx);
        if (!nextBasicExpr) {
            std::cerr << "Failed to parse an expression!" << std::endl;
            return {};
        }

        subExprs.push_back(std::move(*nextBasicExpr));
    }

    assert(!subExprs.empty());
    assert(subExprs.size() == operators.size() + 1);

    // Addition and substraction have the same priority, so the resulting tree has
    // a trivial structure.
    auto lhs = std::move(subExprs.front());
    for (auto i = size_t{}; i < operators.size(); ++i) {
        auto rhs = std::move(subExprs.at(i + 1));
        auto op = operators.at(i);

        auto operands = std::vector<AstNodeExpr>{std::move(lhs), std::move(rhs)};
        if (std::holds_alternative<TokenPlus>(op)) {
            auto newLhs = AstNodeAddition{.operands = std::move(operands)};
            lhs = AstNodeExpr{std::move(newLhs)};
        } else if (std::holds_alternative<TokenMinus>(op)) {
            auto newLhs = AstNodeSubstraction{.operands = std::move(operands)};
            lhs = AstNodeExpr{std::move(newLhs)};
        } else {
            std::cerr << "Expected a binary operation, got " << printToken(op) << std::endl;
            assert(false);
        }
    }
    return lhs;
}

std::optional<AstNodeStmt> consumeAssignment(ParserContext &ctx) {
    auto &tokens = ctx.tokens;

    if (!peek<TokenIdentifier, TokenAssignment>(tokens)) {
        std::cerr << "Assignment must start with a variable name and an assignment operator." << std::endl;
        return {};
    }

    auto variable = std::optional<TokenIdentifier>{};
    std::tie(variable, tokens) = consume<TokenIdentifier>(tokens);
    assert(variable.has_value());
    std::tie(std::ignore, tokens) = consume<TokenAssignment>(tokens);

    auto expr = consumeExpression(ctx);
    if (!expr) {
        std::cerr << "Expected an expression" << std::endl;
        return {};
    }

    return AstNodeAssignment{.variable = std::move(*variable), .value = std::move(*expr)};
}

std::optional<AstNodeStmt> consumeStatement(ParserContext &ctx) {
    auto &tokens = ctx.tokens;

    if (peek<TokenIdentifier, TokenAssignment>(tokens)) {
        return consumeAssignment(ctx);
    } else if (peek<TokenIdentifier, TokenLBrace>(tokens)) {
        auto funcCall = consumeFunctionCall(ctx);
        if (!funcCall) {
            return {};
        }
        return AstNodeStmt{*funcCall};
    }

    std::cerr << "Failed to parse a statement" << std::endl;
    return {};
}

void parseSourceFile(ParserContext &ctx) {
    auto &tokens = ctx.tokens;
    auto &ast = ctx.ast;

    while (!tokens.empty()) {
        auto tokensSizeBeforeParse = tokens.size();

        auto stmt = consumeStatement(ctx);
        if (stmt.has_value()) {
            ast.push_back(*stmt);
        }

        if (tokens.size() == tokensSizeBeforeParse) {
            std::cerr << "Failed to consume any input! Skipping this token: " << printToken(tokens.front())
                      << std::endl;
            tokens = tokens.subspan(1);
        }
    }
}

} // namespace

Ast parseSourceFile(TokensSpan tokens) {
    auto ctx = ParserContext{.tokens = tokens, .ast = {}};
    parseSourceFile(ctx);
    return std::move(ctx.ast);
}
