#pragma once

#include <span>

#include "ast.h"
#include "tokens.h"

using TokensSpan = std::span<const Token>;

using ParserOutput = ProgramDescription;

ParserOutput parseSourceFile(TokensSpan tokens);
