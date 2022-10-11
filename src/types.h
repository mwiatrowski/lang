#pragma once

#include <string>
#include <string_view>
#include <variant>
#include <vector>

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

struct Member;
struct Struct {
    std::vector<Member> members;

    auto operator<=>(const Struct &) const = default;
};

struct Type : public std::variant<I64, Bool, String, Function, Struct> {};

struct Member {
    std::string_view name;
    type::Type type;

    auto operator<=>(Member const &) const = default;
};

} // namespace type

std::string printType(const type::Type &type);
