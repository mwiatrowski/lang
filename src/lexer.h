#pragma once

#include <string_view>
#include <vector>

#include "tokens.h"

std::vector<Token> lexSourceCode(std::string_view input);
