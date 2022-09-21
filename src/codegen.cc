#include "codegen.h"

#include <cassert>
#include <iostream>
#include <sstream>

#include "scope.h"
#include "variant_helpers.h"

namespace {

// TODO don't use global (maybe?)
std::string getTmpVarName() {
    static auto tempVarCount = int{0};
    return "__tmp_" + std::to_string(tempVarCount++);
}

constexpr auto *STD_LIB_CONTENTS = R"STDLIB_RAWSTRING(

#include <iostream>
#include <sstream>
#include <string>

template<typename... Ts>
void print(Ts&&... args) {
  auto output = std::stringstream{};
  ((output << std::forward<Ts>(args) << ' '), ...);
  output << "\n";
  std::cout << output.str() << std::flush;
}

)STDLIB_RAWSTRING";

std::string writeTemporaryAssignment(std::ostream &output, const AstNodeExpr &expr);

std::string generateFuncCallStr(std::ostream &output, const AstNodeFuncCall &funcCall) {
    auto args = std::vector<std::string>{};
    for (const auto &argExpr : funcCall.arguments) {
        args.emplace_back(writeTemporaryAssignment(output, argExpr));
    }

    auto funcCallStr = std::stringstream{};
    funcCallStr << funcCall.functionName.name << "(";
    for (size_t i = 0; i < args.size(); ++i) {
        if (i != 0) {
            funcCallStr << ", ";
        }
        funcCallStr << args.at(i);
    }
    funcCallStr << ")";

    return funcCallStr.str();
}

std::string writeTemporaryAssignment(std::ostream &output, const AstNodeExpr &expr) {
    auto writeDecl = [&output](const auto &value) -> std::string {
        auto name = getTmpVarName();
        output << "auto " << name << " = " << value << ";\n";
        return name;
    };

    if (const auto literal = to<AstNodeIntLiteral>(expr)) {
        return writeDecl(literal->value.value);
    }

    if (const auto literal = to<AstNodeStringLiteral>(expr)) {
        auto escapedStr = "R\"IMPL_STR_LITERAL(" + std::string{literal->value.value} + ")IMPL_STR_LITERAL\"";
        return writeDecl(escapedStr);
    }

    if (const auto identifier = to<AstNodeIdentifier>(expr)) {
        return writeDecl(identifier->value.name);
    }

    if (const auto funcCall = to<AstNodeFuncCall>(expr)) {
        auto funcCallStr = generateFuncCallStr(output, *funcCall);
        return writeDecl(funcCallStr);
    }

    if (const auto addition = to<AstNodeAddition>(expr)) {
        assert(addition->operands.size() == 2);
        auto lhs = writeTemporaryAssignment(output, addition->operands[0]);
        auto rhs = writeTemporaryAssignment(output, addition->operands[1]);
        return writeDecl(lhs + " + " + rhs);
    }

    if (const auto substraction = to<AstNodeSubstraction>(expr)) {
        assert(substraction->operands.size() == 2);
        auto lhs = writeTemporaryAssignment(output, substraction->operands[0]);
        auto rhs = writeTemporaryAssignment(output, substraction->operands[1]);
        return writeDecl(lhs + " - " + rhs);
    }

    if (const auto negation = to<AstNodeNegation>(expr)) {
        assert(negation->operands.size() == 1);
        auto rhs = writeTemporaryAssignment(output, negation->operands[0]);
        return writeDecl("-" + rhs);
    }

    if (const auto funcDef = to<AstNodeFuncRef>(expr)) {
        std::cerr << "Generating function definitions is not supported yet." << std::endl;
        return writeDecl("nullptr");
    }

    std::cerr << "Unexpected expression type: " << expr.index() << std::endl;
    assert(false);
}

void writeFunctionCall(std::ostream &output, const AstNodeFuncCall &funcCall) {
    auto funcCallStr = generateFuncCallStr(output, funcCall);
    output << "(void) " << funcCallStr << ";\n";
}

void writeAssignment(std::ostream &output, const AstNodeAssignment &assignment) {
    const auto &varName = assignment.variable.name;
    auto tmpVar = writeTemporaryAssignment(output, assignment.value);
    output << "auto " << varName << " = " << tmpVar << ";\n";
}

void writeMain(std::ostream &output, const Ast &ast) {
    output << "int main() {\n";
    auto endMain = defer([&] { output << "}\n"; });

    for (const auto &stmt : ast) {
        if (const auto &assignment = to<AstNodeAssignment>(stmt)) {
            writeAssignment(output, *assignment);
        } else if (const auto &funcCall = to<AstNodeFuncCall>(stmt)) {
            writeFunctionCall(output, *funcCall);
        } else {
            std::cerr << "Unexpected statement type (index: " << stmt.index() << ")" << std::endl;
            assert(false);
        }
    }
}

} // namespace

std::string generateCode(const Ast &ast) {
    auto output = std::stringstream{};
    output << STD_LIB_CONTENTS;
    writeMain(output, ast);
    return output.str();
}
