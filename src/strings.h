#pragma once

#include <optional>
#include <string_view>
#include <vector>

std::vector<std::string_view> splitStr(std::string_view input, char separator);
std::pair<std::string_view, std::string_view> cutStr(std::string_view input, size_t pos);
std::optional<std::string_view> removePrefix(std::string_view input, std::string_view prefix);

std::string_view consumeWhitespace(std::string_view input);
std::string_view consumeWhitespaceBack(std::string_view input);
std::string_view trimStr(std::string_view input);
