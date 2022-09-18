#pragma once

#include <optional>
#include <variant>

template <typename T, typename... As> std::optional<T> to(const std::variant<As...> &taggedUnion) {
    if (std::holds_alternative<T>(taggedUnion)) {
        return std::get<T>(taggedUnion);
    }
    return {};
}
