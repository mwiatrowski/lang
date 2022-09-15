#pragma once

#include <unordered_map>
#include <variant>

#include "parser.h"

namespace type {

struct I64 {
  auto operator<=>(const I64 &) const = default;
};

struct String {
  auto operator<=>(const String &) const = default;
};

} // namespace type

using Type = std::variant<type::I64, type::String>;

std::string printType(const Type &type);

using TypeInfo = std::unordered_map<std::string_view, Type>;

TypeInfo resolveTypes(const Ast &ast);
