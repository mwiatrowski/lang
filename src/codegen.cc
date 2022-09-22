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
#include <tuple>

template<typename... Ts>
void print(Ts&&... args) {
  auto output = std::stringstream{};
  ((output << std::forward<Ts>(args) << ' '), ...);
  output << "\n";
  std::cout << output.str() << std::flush;
}

)STDLIB_RAWSTRING";

void writeStdLib(std::ostream &output) { output << STD_LIB_CONTENTS; }

std::string typeNameToCppTypeName(TokenIdentifier typeName) {
    const auto &name = typeName.name;

    if (name == "int") {
        return "int64_t";
    }

    if (name == "str") {
        return "std::string";
    }

    return std::string{name};
}

std::string returnTypeToCppTypeName(std::vector<TypedVariable> const &returnTypes) {
    auto typesNum = std::ssize(returnTypes);

    if (typesNum == 0) {
        return "void";
    }

    if (typesNum == 1) {
        const auto &[argName, argType] = returnTypes.front();
        return typeNameToCppTypeName(argType);
    }

    auto out = std::stringstream{};
    out << "std::tuple<";
    for (auto i = 0; i < typesNum; ++i) {
        if (i > 0) {
            out << ", ";
        }
        auto const &[argName, argType] = returnTypes.at(i);
        out << typeNameToCppTypeName(argType);
    }
    out << ">";
    return out.str();
}

void writeReturnStatement(std::ostream &output, std::vector<TypedVariable> const &returnVals) {
    auto retValsNum = std::ssize(returnVals);

    if (retValsNum == 0) {
        output << "return;" << std::endl;
        return;
    }

    if (retValsNum == 1) {
        auto const &[varName, varType] = returnVals.front();
        output << "return " << varName.name << ";" << std::endl;
        return;
    }

    output << "return std::make_tuple(";
    for (auto i = 0; i < retValsNum; ++i) {
        if (i > 0) {
            output << ", ";
        }
        auto const &[argName, argType] = returnVals.at(i);
        output << argName.name;
    }
    output << ");" << std::endl;
}

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
        return writeDecl(funcDef->generatedName);
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

void writeSingleFunctionBody(std::ostream &output, const Ast &ast) {
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

void writeFunctionHeader(std::ostream &output, const std::string &name, const FunctionDefinition &func) {
    output << returnTypeToCppTypeName(func.returnVals) << " " << name << "(";
    for (auto i = 0; i < ssize(func.arguments); ++i) {
        if (i > 0) {
            output << ", ";
        }
        auto const &[argName, argType] = func.arguments.at(i);
        output << typeNameToCppTypeName(argType) << " " << argName.name;
    }
    output << ")";
}

void writeFunctionDeclaration(std::ostream &output, const std::string &name, const FunctionDefinition &func) {
    writeFunctionHeader(output, name, func);
    output << ";" << std::endl;
}

void writeFunctionDefinition(std::ostream &output, const std::string &name, const FunctionDefinition &func) {
    writeFunctionHeader(output, name, func);
    output << " {" << std::endl;
    writeSingleFunctionBody(output, func.functionBody);
    writeReturnStatement(output, func.returnVals);
    output << "}" << std::endl;
}

void writeUserFunctions(std::ostream &output, const FuncDefs &funcDefs) {
    (void)funcDefs;

    auto declarations = std::stringstream{};
    auto definitions = std::stringstream{};

    for (const auto &[name, def] : funcDefs) {
        writeFunctionDeclaration(declarations, name, def);
        writeFunctionDefinition(definitions, name, def);
    }

    output << declarations.str() << std::endl;
    output << definitions.str() << std::endl;
}

void writeMain(std::ostream &output, const Ast &ast) {
    output << "int main() {\n";
    writeSingleFunctionBody(output, ast);
    output << "}\n";
}

} // namespace

std::string generateCode(const ParserOutput &parserOutput) {
    auto output = std::stringstream{};
    writeStdLib(output);
    writeUserFunctions(output, parserOutput.functions);
    writeMain(output, parserOutput.ast);
    return output.str();
}
