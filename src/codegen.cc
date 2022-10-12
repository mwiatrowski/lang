#include "codegen.h"

#include <cassert>
#include <iostream>
#include <ranges>
#include <sstream>
#include <string>
#include <unordered_set>

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

struct __empty_t {};

template<typename... Ts>
__empty_t print(Ts&&... args) {
    auto output = std::stringstream{};
    ((output << std::forward<Ts>(args)), ...);
    std::cout << output.str() << std::flush;
    return {};
}

template<typename... Ts>
__empty_t println(Ts&&... args) {
    print(std::forward<Ts>(args) ..., '\n');
    return {};
}

std::string input() {
    auto result = std::string{};
    std::cin >> result;
    return result;
}

)STDLIB_RAWSTRING";

using DeclaredVars = std::unordered_set<std::string>;

void writeStdLib(std::ostream &output) { output << STD_LIB_CONTENTS; }

std::string typeNameToCppTypeName(TokenIdentifier typeName) {
    const auto &name = typeName.name;

    if (name == "int") {
        return "int64_t";
    }

    if (name == "str") {
        return "std::string";
    }

    if (name == "bool") {
        return "bool";
    }

    return std::string{name};
}

std::string returnTypeToCppTypeName(std::vector<ast::TypedVariable> const &returnTypes) {
    auto typesNum = std::ssize(returnTypes);

    if (typesNum == 0) {
        return "__empty_t";
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

void writeRetValDeclaration(std::ostream &output, DeclaredVars &declared,
                            std::vector<ast::TypedVariable> const &returnVals) {
    for (auto const &[retName, retType] : returnVals) {
        output << "auto " << retName.name << " = " << typeNameToCppTypeName(retType) << "{};\n";
        declared.insert(std::string{retName.name});
    }
}

void writeReturnStatement(std::ostream &output, std::vector<ast::TypedVariable> const &returnVals) {
    auto retValsNum = std::ssize(returnVals);

    if (retValsNum == 0) {
        output << "return {};" << std::endl;
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

std::string writeTemporaryAssignment(std::ostream &output, const ast::Expr &expr, bool isRef);

std::string generateFuncCallStr(std::ostream &output, const ast::FuncCall &funcCall) {
    auto args = std::vector<std::string>{};
    for (const auto &argExpr : funcCall.arguments) {
        args.emplace_back(writeTemporaryAssignment(output, argExpr, false));
    }

    if (!is<ast::Identifier>(*funcCall.object)) {
        std::cerr << "Right now complex expressions can't be called." << std::endl;
        return "INVALID";
    }
    auto const &funcName = as<ast::Identifier>(*funcCall.object);

    auto funcCallStr = std::stringstream{};
    funcCallStr << funcName.value.name << "(";
    for (size_t i = 0; i < args.size(); ++i) {
        if (i != 0) {
            funcCallStr << ", ";
        }
        funcCallStr << args.at(i);
    }
    funcCallStr << ")";

    return funcCallStr.str();
}

char const *getBinaryOperationStr(Token const &op) {
    if (to<TokenPlus>(op)) {
        return "+";
    }
    if (to<TokenMinus>(op)) {
        return "-";
    }
    if (to<TokenLess>(op)) {
        return "<";
    }
    if (to<TokenLessOrEqual>(op)) {
        return "<=";
    }
    if (to<TokenGreater>(op)) {
        return ">";
    }
    if (to<TokenGreaterOrEqual>(op)) {
        return ">=";
    }
    if (to<TokenEqual>(op)) {
        return "==";
    }
    if (to<TokenNotEqual>(op)) {
        return "!=";
    }

    std::cerr << "Expected a binary operator, got " << printToken(op) << std::endl;
    assert(false);
}

std::string writeTemporaryAssignment(std::ostream &output, const ast::Expr &expr, bool isRef) {
    auto writeDecl = [&output, &isRef](const auto &value) -> std::string {
        auto name = getTmpVarName();
        output << (isRef ? "auto &" : "auto ") << name << " = " << value << ";\n";
        return name;
    };

    if (const auto literal = to<ast::IntLiteral>(expr)) {
        return writeDecl(literal->value.value);
    }

    if (const auto literal = to<ast::StringLiteral>(expr)) {
        auto escapedStr = "R\"IMPL_STR_LITERAL(" + std::string{literal->value.value} + ")IMPL_STR_LITERAL\"";
        return writeDecl(escapedStr);
    }

    if (is<ast::BoolLiteral>(expr)) {
        auto const &literal = as<ast::BoolLiteral>(expr);
        return writeDecl(literal.value.value ? "true" : "false");
    }

    if (const auto identifier = to<ast::Identifier>(expr)) {
        return writeDecl(identifier->value.name);
    }

    if (const auto funcCall = to<ast::FuncCall>(expr)) {
        auto funcCallStr = generateFuncCallStr(output, *funcCall);
        return writeDecl(funcCallStr);
    }

    if (auto const binaryOp = to<ast::BinaryOp>(expr)) {
        auto lhs = writeTemporaryAssignment(output, *binaryOp->lhs, isRef);
        auto rhs = writeTemporaryAssignment(output, *binaryOp->rhs, isRef);
        auto const *op = getBinaryOperationStr(binaryOp->op);
        return writeDecl(lhs + " " + op + " " + rhs);
    }

    if (const auto negation = to<ast::Negation>(expr)) {
        auto rhs = writeTemporaryAssignment(output, *negation->operand, isRef);
        return writeDecl("-" + rhs);
    }

    if (const auto funcDef = to<ast::FuncRef>(expr)) {
        return writeDecl(funcDef->generatedName);
    }

    if (is<ast::MemberAccess>(expr)) {
        auto const &memAcc = as<ast::MemberAccess>(expr);
        auto obj = writeTemporaryAssignment(output, *memAcc.object, isRef);
        auto mName = std::string{memAcc.member.name};
        return writeDecl(obj + "." + mName);
    }

    std::cerr << "Unexpected expression type: " << expr.index() << std::endl;
    assert(false);
}

void writeDeclaration(std::ostream &output, DeclaredVars &declared, ast::Declaration const &decl) {
    auto const varName = std::string{decl.variable.name};
    if (declared.contains(varName)) {
        std::cerr << "Cannot forward-declare an already declared variable." << std::endl;
        return;
    }

    output << "auto " << varName << " = " << typeNameToCppTypeName(decl.type) << "{};\n";
    declared.insert(varName);
}

void writeAssignment(std::ostream &output, DeclaredVars &declared, const ast::VarAssignment &assignment) {
    auto lhs = std::string{};
    bool declareNewVar = false;

    if (is<ast::Identifier>(assignment.lhs)) {
        auto varName = std::string{as<ast::Identifier>(assignment.lhs).value.name};
        if (!declared.contains(varName)) {
            lhs = varName;
            declareNewVar = true;
            declared.insert(varName);
        }
    }

    if (!declareNewVar) {
        lhs = writeTemporaryAssignment(output, assignment.lhs, true);
    }

    auto rhs = writeTemporaryAssignment(output, assignment.rhs, false);
    output << (declareNewVar ? "auto " : "") << lhs << " = " << rhs << ";\n";
}

void writeStatementList(std::ostream &output, DeclaredVars &declared, const ast::StmtList &stmts);
void writeStatement(std::ostream &output, DeclaredVars &declared, const ast::Stmt &stmt);

void writeScope(std::ostream &output, DeclaredVars &declared, ast::Scope const &scope) {
    auto declaredBefore = declared;
    output << "{\n";
    writeStatementList(output, declared, scope.statements);
    output << "}\n";
    declared = std::move(declaredBefore);
}

void writeIfElifElse(std::ostream &output, DeclaredVars &declared, ast::IfBlock const &ifElifElse) {
    assert(ifElifElse.brIfElif.size() >= 1);

    auto declaredBefore = declared;
    auto nestingLevel = 0;

    for (auto const &[condition, body] : ifElifElse.brIfElif) {
        auto condVar = writeTemporaryAssignment(output, condition, false);
        output << "if (" << condVar << ") {\n";
        writeStatement(output, declared, body);
        output << "} else {\n";
        nestingLevel += 1;
    }

    if (ifElifElse.brElse.hasValue()) {
        auto const &body = *ifElifElse.brElse;
        writeStatement(output, declared, body);
    }

    for (auto i = 0; i < nestingLevel; ++i) {
        output << "}\n";
    }

    declared = std::move(declaredBefore);
}

void writeWhileLoop(std::ostream &output, DeclaredVars &declared, ast::WhileLoop const &loop) {
    auto declaredBefore = declared;

    output << "while (true) {\n";
    auto condVar = writeTemporaryAssignment(output, loop.condition, false);
    output << "if (!" << condVar << ") { break; }\n";
    writeStatement(output, declared, *loop.body);
    output << "}\n";

    declared = std::move(declaredBefore);
}

void writeStatement(std::ostream &output, DeclaredVars &declared, const ast::Stmt &stmt) {
    if (is<ast::StructDecl>(stmt)) {
        // Handled elsewhere.
        return;
    }

    if (is<ast::Declaration>(stmt)) {
        auto const &decl = as<ast::Declaration>(stmt);
        writeDeclaration(output, declared, decl);
        return;
    }

    if (const auto &assignment = to<ast::VarAssignment>(stmt)) {
        writeAssignment(output, declared, *assignment);
        return;
    }

    if (is<ast::Expr>(stmt)) {
        auto const &expr = as<ast::Expr>(stmt);
        writeTemporaryAssignment(output, expr, false);
        return;
    }

    if (auto const &scope = to<ast::Scope>(stmt)) {
        writeScope(output, declared, *scope);
        return;
    }

    if (is<ast::IfBlock>(stmt)) {
        auto const &ifElifElse = as<ast::IfBlock>(stmt);
        writeIfElifElse(output, declared, ifElifElse);
        return;
    }

    if (is<ast::WhileLoop>(stmt)) {
        auto const &loop = as<ast::WhileLoop>(stmt);
        writeWhileLoop(output, declared, loop);
        return;
    }

    if (is<ast::BreakStmt>(stmt)) {
        output << "break;\n";
        return;
    }

    if (is<ast::ContinueStmt>(stmt)) {
        output << "continue;\n";
        return;
    }

    std::cerr << "Unexpected statement type (index: " << stmt.index() << ")" << std::endl;
    assert(false);
}

void writeStatementList(std::ostream &output, DeclaredVars &declared, const ast::StmtList &stmts) {
    for (const auto &stmt : stmts) {
        writeStatement(output, declared, stmt);
    }
}

void writeFunctionHeader(std::ostream &output, const std::string &name, const ast::FunctionDefinition &func) {
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

void writeFunctionDeclaration(std::ostream &output, const std::string &name, const ast::FunctionDefinition &func) {
    writeFunctionHeader(output, name, func);
    output << ";" << std::endl;
}

void writeFunctionDefinition(std::ostream &output, const std::string &name, const ast::FunctionDefinition &func) {
    writeFunctionHeader(output, name, func);

    auto declared = DeclaredVars{};
    output << " {" << std::endl;
    writeRetValDeclaration(output, declared, func.returnVals);
    writeStatement(output, declared, func.functionBody);
    writeReturnStatement(output, func.returnVals);
    output << "}" << std::endl;
}

void writeStruct(std::ostream &output, ast::StructDecl const &structDecl) {
    auto const &name = structDecl.name.name;
    auto const &members = structDecl.definition.members;

    output << "struct " << name << "{\n";
    for (auto const &[varName, varType] : members) {
        output << typeNameToCppTypeName(varType) << " " << varName.name << ";\n";
    }
    output << "};\n";
}

void writeUserStructs(std::ostream &output, ast::StmtList const &stmts) {
    auto structs = std::span(stmts.begin(), stmts.end()) |
                   std::views::filter([](auto const &stmt) { return is<ast::StructDecl>(stmt); }) |
                   std::views::transform([](auto const &stmt) { return as<ast::StructDecl>(stmt); });

    for (auto const &structDecl : structs) {
        writeStruct(output, structDecl);
    }
}

void writeUserFunctions(std::ostream &output, const ast::FuncDefs &funcDefs) {
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

void writeMain(std::ostream &output, const ast::StmtList &ast) {
    auto declared = DeclaredVars{};
    output << "int main() {\n";
    writeStatementList(output, declared, ast);
    output << "}\n";
}

} // namespace

std::string generateCode(const ParserOutput &parserOutput) {
    auto output = std::stringstream{};
    writeStdLib(output);
    writeUserStructs(output, parserOutput.ast);
    writeUserFunctions(output, parserOutput.functions);
    writeMain(output, parserOutput.ast);
    return output.str();
}
