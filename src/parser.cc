#include "parser.h"

#include <cassert>
#include <concepts>
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
    ast::FuncDefs functions;
    int nextId{0};
};

std::string generateNewFunctionName(ParserContext &ctx) { return "__temp_func_" + std::to_string(ctx.nextId++); }

using TypedArgList = decltype(ast::FunctionDefinition::arguments);

template <typename FnConsume> using ConsumedType = std::invoke_result<FnConsume, ParserContext &>::type::value_type;

std::optional<ast::Expr> consumeExpression(ParserContext &ctx);
std::optional<ast::Stmt> consumeStatement(ParserContext &ctx);

template <typename TokenType> std::optional<TokenType> consumeToken(ParserContext &ctx) {
    auto &tokens = ctx.tokens;

    if (!peek<TokenType>(tokens)) {
        return {};
    }

    auto [head, tail] = consume<TokenType>(tokens);
    tokens = tail;
    return head;
}

template <typename... TokenType> std::optional<Token> consumeTokenAnyOf(ParserContext &ctx) {
    auto result = std::optional<Token>{};
    ((result = consumeToken<TokenType>(ctx)) || ...);
    return result;
}

template <typename FnGetElem, typename FnGetSep>
requires std::invocable<FnGetElem, ParserContext &> && std::invocable<FnGetSep, ParserContext &>
auto consumeDelimited(ParserContext &ctx, FnGetElem consumeElem, FnGetSep consumeSep)
    -> std::vector<ConsumedType<FnGetElem>> {
    using ElemType = ConsumedType<FnGetElem>;
    auto result = std::vector<ElemType>{};

    while (true) {
        if (auto elem = consumeElem(ctx)) {
            result.push_back(std::move(*elem));
        } else {
            break;
        }

        if (!consumeSep(ctx)) {
            break;
        }
    }

    return result;
}

template <typename FnGetElem, typename FnGetSep>
requires std::invocable<FnGetElem, ParserContext &> && std::invocable<FnGetSep, ParserContext &>
auto consumeDelimitedNoTrailing(ParserContext &ctx, FnGetElem consumeElem, FnGetSep consumeSep)
    -> std::vector<ConsumedType<FnGetElem>> {
    using ElemType = ConsumedType<FnGetElem>;
    auto result = std::vector<ElemType>{};

    if (auto elem = consumeElem(ctx)) {
        result.push_back(std::move(*elem));
    } else {
        return {};
    }

    while (true) {
        if (!consumeSep(ctx)) {
            break;
        }

        if (auto elem = consumeElem(ctx)) {
            result.push_back(std::move(*elem));
        } else {
            std::cerr << "Trailing delimiter is not allowed." << std::endl;
            break;
        }
    }

    return result;
}

std::optional<ast::Expr> consumeParenthesizedExpression(ParserContext &ctx) {
    if (!consumeToken<TokenLBrace>(ctx)) {
        std::cerr << "Expected an opening brace." << std::endl;
        return {};
    }

    auto expr = consumeExpression(ctx);
    if (!expr) {
        std::cerr << "Expected an expression." << std::endl;
        return {};
    }

    if (!consumeToken<TokenRBrace>(ctx)) {
        std::cerr << "Expected a closing brace." << std::endl;
        return {};
    }

    return expr;
}

std::optional<ast::TypedVariable> consumeTypedVariable(ParserContext &ctx) {
    auto &tokens = ctx.tokens;

    if (!peek<TokenIdentifier, TokenColon, TokenIdentifier>(tokens)) {
        return {};
    }

    auto variable = consumeToken<TokenIdentifier>(ctx);
    assert(consumeToken<TokenColon>(ctx));
    auto typeName = consumeToken<TokenIdentifier>(ctx);

    assert(variable);
    assert(typeName);

    return ast::TypedVariable{.varName = std::move(*variable), .varType = std::move(*typeName)};
}

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

std::optional<ast::FuncRef> consumeFunctionDefinition(ParserContext &ctx) {
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

    auto funcBody = consumeStatement(ctx);
    if (!funcBody) {
        std::cerr << "Expected a statement." << std::endl;
        return {};
    }

    auto funcDef = ast::FunctionDefinition{
        .arguments = std::move(*funcArgs), .returnVals = std::move(*funcRetVals), .functionBody = std::move(*funcBody)};
    auto funcName = generateNewFunctionName(ctx);
    ctx.functions[funcName] = std::move(funcDef);

    return ast::FuncRef{.generatedName = funcName};
}

std::optional<ast::StructDef> consumeStructDefinition(ParserContext &ctx) {
    auto &tokens = ctx.tokens;

    if (!peek<TokenKwStruct, TokenLCurBrace>(tokens)) {
        std::cerr << "Expected a structure definition." << std::endl;
        return {};
    }
    assert(consumeToken<TokenKwStruct>(ctx));
    assert(consumeToken<TokenLCurBrace>(ctx));

    auto members = consumeDelimited(ctx, consumeTypedVariable, consumeToken<TokenComma>);

    if (!consumeToken<TokenRCurBrace>(ctx)) {
        std::cerr << "Expected a closing curly brace." << std::endl;
        return {};
    }

    return ast::StructDef{.members = std::move(members)};
}

std::optional<ast::Scope> consumeScopedStmtList(ParserContext &ctx) {
    auto &tokens = ctx.tokens;

    if (!peek<TokenLCurBrace>(tokens)) {
        std::cerr << "Expected an opening curly brace." << std::endl;
        return {};
    }
    std::tie(std::ignore, tokens) = consume<TokenLCurBrace>(tokens);

    auto stmts = std::vector<ast::Stmt>{};
    while (!peek<TokenRCurBrace>(tokens)) {
        auto stmt = consumeStatement(ctx);
        if (!stmt) {
            std::cerr << "Expected a statement or a closing curly brace." << std::endl;
            tokens = consumeUntil<TokenRCurBrace>(tokens);
            return {};
        }
        stmts.push_back(std::move(*stmt));
    }

    assert(peek<TokenRCurBrace>(tokens));
    std::tie(std::ignore, tokens) = consume<TokenRCurBrace>(tokens);

    return ast::Scope{.statements = std::move(stmts)};
}

std::optional<std::vector<ast::Expr>> consumeFunctionCallArguments(ParserContext &ctx) {
    auto &tokens = ctx.tokens;

    if (!peek<TokenLBrace>(tokens)) {
        std::cerr << "Expected an opening brace." << std::endl;
        return {};
    }
    assert(consumeToken<TokenLBrace>(ctx));

    auto arguments = consumeDelimited(ctx, consumeExpression, consumeToken<TokenComma>);

    if (!peek<TokenRBrace>(tokens)) {
        std::cerr << "Expected a closing brace." << std::endl;
        return {};
    }
    assert(consumeToken<TokenRBrace>(ctx));

    return {std::move(arguments)};
}

std::optional<ast::Expr> consumeAtomExpression(ParserContext &ctx) {
    auto &tokens = ctx.tokens;

    if (peek<TokenLBrace>(tokens)) {
        return consumeParenthesizedExpression(ctx);
    }

    if (peek<TokenIdentifier>(tokens)) {
        auto [tokenIdentifier, tokensTail] = consume<TokenIdentifier>(tokens);
        tokens = tokensTail;
        return ast::Expr{ast::Identifier{.value = tokenIdentifier}};
    }

    if (peek<TokenIntLiteral>(tokens)) {
        auto [tokenLiteral, tokensTail] = consume<TokenIntLiteral>(tokens);
        tokens = tokensTail;
        return ast::Expr{ast::IntLiteral{.value = tokenLiteral}};
    }

    if (peek<TokenStringLiteral>(tokens)) {
        auto [tokenLiteral, tokensTail] = consume<TokenStringLiteral>(tokens);
        tokens = tokensTail;
        return ast::Expr{ast::StringLiteral{.value = tokenLiteral}};
    }

    if (peek<TokenBoolLiteral>(tokens)) {
        auto tokenLiteral = consumeToken<TokenBoolLiteral>(ctx);
        assert(tokenLiteral.has_value());
        return ast::Expr{ast::BoolLiteral{.value = *tokenLiteral}};
    }

    // TODO: Function definitions should be handled more like structs.
    if (peek<TokenKwFn>(tokens)) {
        auto funcDef = consumeFunctionDefinition(ctx);
        return funcDef.has_value() ? std::optional<ast::Expr>{*funcDef} : std::optional<ast::Expr>{};
    }

    return {};
}

std::optional<ast::Expr> consumeBasicExpression(ParserContext &ctx) {
    auto &tokens = ctx.tokens;

    if (peek<TokenMinus>(tokens)) {
        assert(consumeToken<TokenMinus>(ctx));
        auto expr = consumeBasicExpression(ctx);
        if (!expr) {
            std::cerr << "Expected an expression after a unary minus" << std::endl;
            return {};
        }
        return ast::Expr{ast::Negation{.operand = {std::move(*expr)}}};
    }

    auto resultExpr = consumeAtomExpression(ctx);
    if (!resultExpr) {
        std::cerr << "Failed to parse an expression." << std::endl;
        return {};
    }

    while (true) {
        if (peek<TokenDot, TokenIdentifier>(tokens)) {
            assert(consumeToken<TokenDot>(ctx));
            auto memberName = consumeToken<TokenIdentifier>(ctx);
            assert(memberName);

            auto memAcc = ast::MemberAccess{.object = {std::move(*resultExpr)}, .member = std::move(*memberName)};
            resultExpr = ast::Expr{std::move(memAcc)};

            continue;
        }

        if (peek<TokenLBrace>(tokens)) {
            auto args = consumeFunctionCallArguments(ctx);
            if (!args) {
                std::cerr << "Expected a list of arguments for the function call." << std::endl;
                return {};
            }

            auto funcCall = ast::FuncCall{.object = {std::move(*resultExpr)}, .arguments = std::move(*args)};
            resultExpr = ast::Expr{std::move(funcCall)};

            continue;
        }

        break;
    }

    assert(resultExpr);
    return resultExpr;
}

std::optional<Token> consumeBinaryOperator(ParserContext &ctx) {
    return consumeTokenAnyOf<TokenPlus, TokenMinus, TokenLess, TokenLessOrEqual, TokenGreater, TokenGreaterOrEqual,
                             TokenEqual, TokenNotEqual>(ctx);
}

std::optional<ast::Expr> consumeExpression(ParserContext &ctx) {
    auto &tokens = ctx.tokens;

    auto firstBasicExpr = consumeBasicExpression(ctx);
    if (!firstBasicExpr) {
        std::cerr << "Failed to parse an expression!" << std::endl;
        return {};
    }

    auto subExprs = std::vector<ast::Expr>{*firstBasicExpr};
    auto operators = std::vector<Token>{};

    while (!tokens.empty()) {
        auto op = consumeBinaryOperator(ctx);
        if (!op) {
            break;
        }

        operators.push_back(*op);

        auto nextBasicExpr = consumeBasicExpression(ctx);
        if (!nextBasicExpr) {
            std::cerr << "Failed to parse an expression!" << std::endl;
            return {};
        }

        subExprs.push_back(std::move(*nextBasicExpr));
    }

    assert(!subExprs.empty());
    assert(subExprs.size() == operators.size() + 1);

    // For now, assume that all operators have the same precedence.
    // This will change in the future.
    auto lhs = std::move(subExprs.front());
    for (auto i = size_t{}; i < operators.size(); ++i) {
        auto rhs = std::move(subExprs.at(i + 1));
        auto op = operators.at(i);

        auto newLhs = ast::BinaryOp{.op = op, .lhs = {std::move(lhs)}, .rhs = {std::move(rhs)}};
        lhs = ast::Expr{std::move(newLhs)};
    }
    return lhs;
}

std::optional<ast::Stmt> consumeDeclaration(ParserContext &ctx) {
    auto tVar = consumeTypedVariable(ctx);
    if (!tVar) {
        std::cerr << "Expected a variable declaration." << std::endl;
        return {};
    }
    return ast::Stmt{ast::Declaration{.variable = std::move(tVar->varName), .type = std::move(tVar->varType)}};
}

std::optional<ast::Stmt> consumeAssignmentOrExpression(ParserContext &ctx) {
    auto lhs = consumeExpression(ctx);
    if (!lhs) {
        std::cerr << "Expected an assignment left-hand side." << std::endl;
        return {};
    }

    if (!consumeToken<TokenAssignment>(ctx)) {
        return ast::Stmt{std::move(*lhs)};
    }

    auto rhs = consumeExpression(ctx);
    if (!rhs) {
        std::cerr << "Expected an assignment right-hand side." << std::endl;
        return {};
    }

    return ast::Stmt{ast::VarAssignment{.lhs = std::move(*lhs), .rhs = std::move(*rhs)}};
}

std::optional<ast::Stmt> consumeStructDeclaration(ParserContext &ctx) {
    auto &tokens = ctx.tokens;

    if (!peek<TokenIdentifier, TokenAssignment, TokenKwStruct>(tokens)) {
        std::cerr << "Expected a struct declaration." << std::endl;
        return {};
    }

    auto name = consumeToken<TokenIdentifier>(ctx);
    assert(name.has_value());
    assert(consumeToken<TokenAssignment>(ctx));

    auto structDef = consumeStructDefinition(ctx);
    if (!structDef) {
        std::cerr << "Expected a struct definition." << std::endl;
        return {};
    }

    auto structDecl = ast::StructDecl{.name = std::move(*name), .definition = std::move(*structDef)};
    return ast::Stmt{std::move(structDecl)};
}

template <typename InitialKeyword>
requires(std::same_as<InitialKeyword, TokenKwIf> ||
         std::same_as<InitialKeyword, TokenKwElif>) std::optional<ast::Branch> consumeIfelifBranch(ParserContext &ctx) {
    auto kw = consumeToken<InitialKeyword>(ctx);
    if (!kw) {
        std::cerr << "Expected " << printToken(InitialKeyword{}) << std::endl;
        return {};
    }

    auto condition = consumeExpression(ctx);
    if (!condition) {
        std::cerr << "Expected an branch condition." << std::endl;
        return {};
    }

    auto body = consumeStatement(ctx);
    if (!body) {
        std::cerr << "Expected a branch body." << std::endl;
        return {};
    }

    return ast::Branch{std::move(*condition), std::move(*body)};
}

std::optional<ast::Stmt> consumeElseBranch(ParserContext &ctx) {
    auto kw = consumeToken<TokenKwElse>(ctx);
    if (!kw) {
        std::cerr << "Expected " << printToken(TokenKwElse{}) << std::endl;
        return {};
    }

    auto body = consumeStatement(ctx);
    if (!body) {
        std::cerr << "Expected a branch body." << std::endl;
        return {};
    }

    return body;
}

std::optional<ast::IfBlock> consumeIfElifElse(ParserContext &ctx) {
    auto &tokens = ctx.tokens;

    auto brIfElif = std::vector<ast::Branch>{};
    auto brElse = ValuePtr<ast::Stmt>{};

    auto ifBranch = consumeIfelifBranch<TokenKwIf>(ctx);
    if (!ifBranch) {
        std::cerr << "Expected an 'if' branch." << std::endl;
        return {};
    }
    brIfElif.push_back(std::move(*ifBranch));

    while (peek<TokenKwElif>(tokens)) {
        auto elifBranch = consumeIfelifBranch<TokenKwElif>(ctx);
        if (!elifBranch) {
            std::cerr << "Expected an 'elif' branch." << std::endl;
            return {};
        }
        brIfElif.push_back(std::move(*elifBranch));
    }

    if (peek<TokenKwElse>(tokens)) {
        auto elseBranch = consumeElseBranch(ctx);
        if (!elseBranch) {
            std::cerr << "Expected an 'else' branch." << std::endl;
            return {};
        }
        brElse = {std::move(*elseBranch)};
    }

    return ast::IfBlock{.brIfElif = std::move(brIfElif), .brElse = std::move(brElse)};
}

std::optional<ast::WhileLoop> consumeWhileLoop(ParserContext &ctx) {
    if (!consumeToken<TokenKwWhile>(ctx)) {
        std::cerr << "Expected the 'while' keyword" << std::endl;
        return {};
    }

    auto condition = consumeExpression(ctx);
    if (!condition) {
        std::cerr << "Expected a loop condition." << std::endl;
        return {};
    }

    auto body = consumeStatement(ctx);
    if (!body) {
        std::cerr << "Expected a loop body." << std::endl;
        return {};
    }

    return ast::WhileLoop{.condition = std::move(*condition), .body = {std::move(*body)}};
}

std::optional<ast::Stmt> consumeStatement(ParserContext &ctx) {
    auto &tokens = ctx.tokens;

    if (peek<TokenIdentifier, TokenColon>(tokens)) {
        return consumeDeclaration(ctx);
    }

    if (peek<TokenLCurBrace>(tokens)) {
        auto scope = consumeScopedStmtList(ctx);
        if (!scope) {
            return {};
        }
        return ast::Stmt{std::move(*scope)};
    }

    if (peek<TokenKwIf>(tokens)) {
        auto ifElifElse = consumeIfElifElse(ctx);
        return ifElifElse.has_value() ? ast::Stmt{std::move(*ifElifElse)} : std::optional<ast::Stmt>{};
    }

    if (peek<TokenKwWhile>(tokens)) {
        auto loop = consumeWhileLoop(ctx);
        return loop ? ast::Stmt{std::move(*loop)} : std::optional<ast::Stmt>{};
    }

    if (peek<TokenKwBreak>(tokens)) {
        consumeToken<TokenKwBreak>(ctx);
        return ast::Stmt{ast::BreakStmt{}};
    }

    if (peek<TokenKwContinue>(tokens)) {
        consumeToken<TokenKwContinue>(ctx);
        return ast::Stmt{ast::ContinueStmt{}};
    }

    if (peek<TokenIdentifier, TokenAssignment, TokenKwStruct>(tokens)) {
        return consumeStructDeclaration(ctx);
    }

    return consumeAssignmentOrExpression(ctx);
}

ast::StmtList consumeStmtList(ParserContext &ctx) {
    auto &tokens = ctx.tokens;
    auto stmts = ast::StmtList{};

    while (!tokens.empty()) {
        auto tokensSizeBeforeParse = tokens.size();

        auto stmt = consumeStatement(ctx);
        if (stmt.has_value()) {
            stmts.push_back(*stmt);
        }

        if (tokens.size() == tokensSizeBeforeParse) {
            std::cerr << "Failed to consume any input! Skipping this token: " << printToken(tokens.front())
                      << std::endl;
            tokens = tokens.subspan(1);
        }
    }

    return stmts;
}

} // namespace

ParserOutput parseSourceFile(TokensSpan tokens) {
    auto ctx = ParserContext{.tokens = tokens, .functions = {}};
    auto ast = consumeStmtList(ctx);
    return ParserOutput{.ast = std::move(ast), .functions = std::move(ctx.functions)};
}
