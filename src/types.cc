#include "types.h"

#include <cassert>
#include <iostream>
#include <optional>
#include <ranges>
#include <sstream>

#include "variant_helpers.h"

namespace {

constexpr auto *BUILTIN_TYPE_INT = "int";
constexpr auto *BUILTIN_TYPE_STR = "str";
constexpr auto *BUILTIN_TYPE_BOOL = "bool";

using TypeDefs = std::unordered_map<std::string, type::Type>;

std::optional<type::Type> getTypeFromName(std::string const &name, TypeDefs const &typeDefs) {
    if (typeDefs.contains(name)) {
        return typeDefs.at(name);
    }

    std::cerr << "Unknown type: " << name << std::endl;
    return {};
}

std::optional<type::Type> getTypeFromName(std::string_view name, TypeDefs const &typeDefs) {
    auto nameStr = std::string{name};
    return getTypeFromName(nameStr, typeDefs);
}

auto getInitialTypeDefs() -> TypeDefs {
    return TypeDefs{{BUILTIN_TYPE_INT, type::Type{type::I64{}}},
                    {BUILTIN_TYPE_STR, type::Type{type::String{}}},
                    {BUILTIN_TYPE_BOOL, type::Type{type::Bool{}}}};
}

auto generateTypeDefs(StmtList const &statements) -> TypeDefs {
    auto structs = std::span(statements.begin(), statements.end()) |
                   std::views::filter([](auto const &stmt) { return is<AstNodeStructDecl>(stmt); }) |
                   std::views::transform([](auto const &stmt) { return as<AstNodeStructDecl>(stmt); });

    auto typeDefs = getInitialTypeDefs();

    // TODO: This scheme doesn't handle out-of-order struct definitions.

    for (auto const &[structNameIdent, structDef] : structs) {
        auto mTypes = std::vector<type::Type>{};
        mTypes.reserve(structDef.members.size());

        for (auto const &member : structDef.members) {
            if (auto type = getTypeFromName(member.varType.name, typeDefs)) {
                mTypes.push_back(*type);
            } else {
                std::cerr << "Couldn't determine the type of a struct member." << std::endl;
                return {};
            }
        }

        auto structName = std::string{structNameIdent.name};
        auto structType = type::Struct{.memberTypes = std::move(mTypes)};
        typeDefs.insert_or_assign(structName, type::Type{std::move(structType)});
    }

    return typeDefs;
}

std::optional<type::Type> getBinaryOperationType(Token const &op, type::Type const &lhsType,
                                                 type::Type const &rhsType) {
    if (lhsType != rhsType) {
        std::cerr << "Binary operation operands have different types, " << printType(lhsType) << " vs "
                  << printType(rhsType) << std::endl;
        return {};
    }

    auto isComparison = [](Token const &op) {
        return anyOf<TokenLess, TokenLessOrEqual, TokenGreater, TokenGreaterOrEqual, TokenEqual, TokenNotEqual>(op);
    };

    if (std::holds_alternative<type::I64>(lhsType)) {
        if (anyOf<TokenPlus, TokenMinus>(op)) {
            return type::Type{type::I64{}};
        }
        if (isComparison(op)) {
            return type::Type{type::Bool{}};
        }
    } else if (std::holds_alternative<type::Bool>(lhsType)) {
        if (anyOf<TokenEqual, TokenNotEqual>(op)) {
            return type::Type{type::Bool{}};
        }
    } else if (std::holds_alternative<type::String>(lhsType)) {
        if (anyOf<TokenPlus>(op)) {
            return type::Type{type::String{}};
        }
    }

    std::cerr << "Unexpected operation " << printToken(op) << " for operands of type " << printType(lhsType)
              << std::endl;
    return {};
}

std::optional<type::Type> getExpressionType(const AstNodeExpr &expr, VarTypes const &varTypes, TypeDefs const &typeDefs,
                                            const FuncDefs &funcDefs) {
    if (std::holds_alternative<AstNodeIntLiteral>(expr)) {
        return type::Type{type::I64{}};
    }

    if (std::holds_alternative<AstNodeStringLiteral>(expr)) {
        return type::Type{type::String{}};
    }

    if (std::holds_alternative<AstNodeBoolLiteral>(expr)) {
        return type::Type{type::Bool{}};
    }

    if (std::holds_alternative<AstNodeIdentifier>(expr)) {
        const auto &identifier = std::get<AstNodeIdentifier>(expr);
        const auto &name = identifier.value.name;

        if (!varTypes.contains(name)) {
            std::cerr << "Type of " << name << " cannot be determined." << std::endl;
            return {};
        }
        return varTypes.at(name);
    }

    if (std::holds_alternative<AstNodeFuncCall>(expr)) {
        const auto &funcCall = std::get<AstNodeFuncCall>(expr);
        std::cerr << "Cannot determine the return type of " << funcCall.functionName.name << std::endl;
        return {};
    }

    if (auto const binaryOp = to<AstNodeBinaryOp>(expr)) {
        const auto lhsType = getExpressionType(binaryOp->operands[0], varTypes, typeDefs, funcDefs);
        const auto rhsType = getExpressionType(binaryOp->operands[1], varTypes, typeDefs, funcDefs);
        if (!lhsType || !rhsType) {
            std::cerr << "Couldn't determine the type of one of the operands." << std::endl;
            return {};
        }

        return getBinaryOperationType(binaryOp->op, *lhsType, *rhsType);
    }

    if (std::holds_alternative<AstNodeNegation>(expr)) {
        const auto &negation = std::get<AstNodeNegation>(expr);
        const auto type = getExpressionType(negation.operands[0], varTypes, typeDefs, funcDefs);
        if (!type || !std::holds_alternative<type::I64>(*type)) {
            std::cerr << "Type of the expression is not integer." << std::endl;
            return {};
        }
        return type::Type{type::I64{}};
    }

    if (const auto &fnRef = to<AstNodeFuncRef>(expr)) {
        auto it = funcDefs.find(fnRef->generatedName);
        assert(it != funcDefs.end());
        const auto &fnDef = it->second;

        auto argTypes = std::vector<type::Type>{};
        for (const auto &[argName, argType] : fnDef.arguments) {
            if (auto type = getTypeFromName(argType.name, typeDefs)) {
                argTypes.push_back(*type);
            } else {
                std::cerr << "Couldn't determine types of arguments." << std::endl;
                return {};
            }
        }

        auto retValTypes = std::vector<type::Type>{};
        for (const auto &[retValName, retValType] : fnDef.returnVals) {
            if (auto type = getTypeFromName(retValType.name, typeDefs)) {
                retValTypes.push_back(*type);
            } else {
                std::cerr << "Couldn't determine types of return values." << std::endl;
                return {};
            }
        }

        return type::Type{type::Function{.inputTypes = std::move(argTypes), .returnTypes = std::move(retValTypes)}};
    }

    std::cerr << "Unexpected expression type!" << std::endl;
    assert(false);
}

} // namespace

std::string printType(const type::Type &type) {
    if (std::holds_alternative<type::I64>(type)) {
        return "INT_64";
    } else if (std::holds_alternative<type::Bool>(type)) {
        return "BOOL";
    } else if (std::holds_alternative<type::String>(type)) {
        return "STRING";
    } else if (const auto &func = to<type::Function>(type)) {
        auto out = std::stringstream{};
        out << "(FUNCTION ( ";
        for (const auto &argType : func->inputTypes) {
            out << printType(argType) << " ";
        }
        out << ") -> ( ";
        for (const auto &retType : func->returnTypes) {
            out << printType(retType) << " ";
        }
        out << "))";
        return out.str();
    } else if (is<type::Struct>(type)) {
        auto const &strct = as<type::Struct>(type);
        auto out = std::stringstream{};

        out << "(STRUCT ( ";
        for (auto const &mType : strct.memberTypes) {
            out << printType(mType) << " ";
        }
        out << "))";

        return out.str();
    }

    std::cerr << "Unexpected type, index: " << type.index() << std::endl;
    assert(false);
}

VarTypes resolveTypes(const ParserOutput &parserOutput) {
    auto varTypes = VarTypes{};
    auto typeDefs = generateTypeDefs(parserOutput.ast);

    for (const auto &stmt : parserOutput.ast) {
        if (is<AstNodeDeclaration>(stmt)) {
            auto const &[varIdent, typeIdent] = as<AstNodeDeclaration>(stmt);

            if (varTypes.contains(varIdent.name)) {
                std::cerr << varIdent.name << " has already been declared." << std::endl;
                continue;
            }

            auto typeNameStr = std::string{typeIdent.name};
            auto type = getTypeFromName(typeNameStr, typeDefs);
            if (!type) {
                std::cerr << "Can't determine the type of " << varIdent.name << ", " << typeNameStr
                          << " does not name a type." << std::endl;
                continue;
            }

            varTypes.insert_or_assign(varIdent.name, *type);
            continue;
        }

        if (is<AstNodeVarAssignment>(stmt)) {
            auto const &assignment = as<AstNodeVarAssignment>(stmt);

            auto const &varName = assignment.variable.name;

            auto type = getExpressionType(assignment.value, varTypes, typeDefs, parserOutput.functions);
            if (!type) {
                std::cerr << "Can't determine the type of " << varName << std::endl;
                continue;
            }

            if (varTypes.contains(varName) && varTypes.at(varName) != *type) {
                std::cerr << varName << " redeclared with a different type (" << printType(varTypes.at(varName))
                          << " vs " << printType(*type) << ")" << std::endl;
                continue;
            }

            varTypes.insert_or_assign(varName, *type);
            continue;
        }
    }

    return varTypes;
}
