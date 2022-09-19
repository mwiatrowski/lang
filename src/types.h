#pragma once

#include <unordered_map>
#include <variant>

#include "parser.h"

namespace type {

struct Type;

struct I64 {
    auto operator<=>(const I64 &) const = default;
};

struct String {
    auto operator<=>(const String &) const = default;
};

struct Function {
    std::vector<Type> inputTypes;
    std::vector<Type> returnTypes;

    auto operator<=>(const Function &) const = default;
};

struct Type : public std::variant<I64, String, Function> {
    using std::variant<I64, String, Function>::variant;
};

} // namespace type

std::string printType(const type::Type &type);

using TypeInfo = std::unordered_map<std::string_view, type::Type>;

TypeInfo resolveTypes(const Ast &ast);
