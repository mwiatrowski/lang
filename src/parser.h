#pragma once

#include <span>

#include "ast.h"
#include "tokens.h"

using TokensSpan = std::span<const Token>;

struct ParserOutput {
    StmtList ast;
    FuncDefs functions;
};

ParserOutput parseSourceFile(TokensSpan tokens);
