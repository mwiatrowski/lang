#pragma once

#include <unordered_map>
#include <variant>

#include "parser.h"

namespace type {

struct Type;

struct I64 {
    auto operator<=>(const I64 &) const = default;
};

struct Bool {
    auto operator<=>(const Bool &) const = default;
};

struct String {
    auto operator<=>(const String &) const = default;
};

struct Function {
    std::vector<Type> inputTypes;
    std::vector<Type> returnTypes;

    auto operator<=>(const Function &) const = default;
};

struct Struct {
    std::vector<Type> memberTypes;

    auto operator<=>(const Struct &) const = default;
};

struct Type : public std::variant<I64, Bool, String, Function, Struct> {};

} // namespace type

std::string printType(const type::Type &type);

using VarTypes = std::unordered_map<std::string_view, type::Type>;
using TypeDefs = std::unordered_map<std::string, type::Type>;

VarTypes resolveTypes(const ParserOutput &parserOutput);
