#pragma once

#include <string_view>
#include <vector>

std::vector<std::string_view> splitStr(std::string_view input, char separator);

std::string_view consumeWhitespace(std::string_view input);
std::string_view consumeWhitespaceBack(std::string_view input);
std::string_view trimStr(std::string_view input);
