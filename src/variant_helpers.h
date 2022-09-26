#pragma once

#include <optional>
#include <variant>

template <typename T, typename... As> std::optional<T> to(const std::variant<As...> &taggedUnion) {
    if (std::holds_alternative<T>(taggedUnion)) {
        return std::get<T>(taggedUnion);
    }
    return {};
}

template <typename... SubTypes, typename... Types> bool anyOf(std::variant<Types...> const &taggedUnion) {
    return (false || ... || std::holds_alternative<SubTypes>(taggedUnion));
}
