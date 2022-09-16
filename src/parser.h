#pragma once

#include <span>

#include "ast.h"
#include "tokens.h"

using TokensSpan = std::span<const Token>;
Ast parseSourceFile(TokensSpan tokens);
