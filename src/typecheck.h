#pragma once

#include <string_view>
#include <unordered_map>

#include "parser.h"
#include "types.h"

using VarTypes = std::unordered_map<std::string_view, type::Type>;

VarTypes resolveTypes(const ParserOutput &parserOutput);
