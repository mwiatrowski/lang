#pragma once

#include <cassert>
#include <optional>
#include <variant>

// TODO Remove uses of this function
template <typename T, typename... As> std::optional<T> to(const std::variant<As...> &taggedUnion) {
    if (std::holds_alternative<T>(taggedUnion)) {
        return std::get<T>(taggedUnion);
    }
    return {};
}

template <typename T, typename... As> bool is(const std::variant<As...> &taggedUnion) {
    return (std::holds_alternative<T>(taggedUnion));
}

template <typename T, typename... As> T const &as(const std::variant<As...> &taggedUnion) {
    assert(is<T>(taggedUnion));
    return std::get<T>(taggedUnion);
}

template <typename... SubTypes, typename... Types> bool anyOf(std::variant<Types...> const &taggedUnion) {
    return (false || ... || std::holds_alternative<SubTypes>(taggedUnion));
}
