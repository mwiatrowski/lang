#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

void startCompilation(const std::string &rootSourceFile) {
  auto sourceStream = std::ifstream(rootSourceFile, std::ios::in);
  auto sourceFileContents =
      std::string(std::istreambuf_iterator<char>(sourceStream),
                  std::istreambuf_iterator<char>());
  std::replace(sourceFileContents.begin(), sourceFileContents.end(), '\n', ' ');

  auto codeStream = std::stringstream{};
  codeStream << "#include <iostream>" << std::endl;
  codeStream << "int main() {" << std::endl;
  codeStream << "std::cout << \"" << sourceFileContents << "\" << std::endl;"
             << std::endl;
  codeStream << "}" << std::endl;

  auto outputStream = std::ofstream("transpiled.cc", std::ios::out);
  outputStream << codeStream.str();
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cerr << "Wrong number of arguments" << std::endl;
    return {};
  }

  auto sourceFile = std::string{argv[1]};
  std::cerr << "Compiling " << sourceFile << std::endl;

  startCompilation(sourceFile);
}
