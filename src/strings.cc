#include "strings.h"

#include <cctype>
#include <string>

std::vector<std::string_view> splitStr(std::string_view input, char separator) {
  auto result = std::vector<std::string_view>{};

  size_t pStart = 0;
  size_t pEnd = input.find(separator);
  while (pEnd != std::string::npos) {
    result.push_back(input.substr(pStart, pEnd - pStart));
    pStart = pEnd + 1;
    pEnd = input.find(separator, pStart);
  }
  result.push_back(input.substr(pStart));

  return result;
}

std::string_view consumeWhitespace(std::string_view input) {
  auto prefixLen = size_t{0};
  while (prefixLen < input.size() && std::isspace(input.at(prefixLen))) {
    prefixLen += 1;
  }
  input.remove_prefix(prefixLen);
  return input;
}

std::string_view consumeWhitespaceBack(std::string_view input) {
  auto suffixLen = size_t{0};
  while (suffixLen < input.size() &&
         std::isspace(input.at(input.size() - 1 - suffixLen))) {
    suffixLen += 1;
  }
  input.remove_suffix(suffixLen);
  return input;
}

std::string_view trimStr(std::string_view input) {
  input = consumeWhitespace(input);
  input = consumeWhitespaceBack(input);
  return input;
}
