#include "types.h"

#include <cassert>
#include <iostream>
#include <optional>
#include <sstream>

#include "variant_helpers.h"

namespace {

std::optional<type::Type> getTypeFromName(std::string_view name) {
    if (name == "int") {
        return type::I64{};
    }

    if (name == "str") {
        return type::String{};
    }

    if (name == "bool") {
        return type::Bool{};
    }

    std::cerr << "Unknown type: " << name << std::endl;
    return {};
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
            return type::I64{};
        }
        if (isComparison(op)) {
            return type::Bool{};
        }
    } else if (std::holds_alternative<type::Bool>(lhsType)) {
        if (anyOf<TokenEqual, TokenNotEqual>(op)) {
            return type::Bool{};
        }
    } else if (std::holds_alternative<type::String>(lhsType)) {
        if (anyOf<TokenPlus>(op)) {
            return type::String{};
        }
    }

    std::cerr << "Unexpected operation " << printToken(op) << " for operands of type " << printType(lhsType)
              << std::endl;
    return {};
}

std::optional<type::Type> getExpressionType(const AstNodeExpr &expr, const TypeInfo &typeInfo,
                                            const FuncDefs &funcDefs) {
    if (std::holds_alternative<AstNodeIntLiteral>(expr)) {
        return type::I64{};
    }

    if (std::holds_alternative<AstNodeStringLiteral>(expr)) {
        return type::String{};
    }

    if (std::holds_alternative<AstNodeBoolLiteral>(expr)) {
        return type::Bool{};
    }

    if (std::holds_alternative<AstNodeIdentifier>(expr)) {
        const auto &identifier = std::get<AstNodeIdentifier>(expr);
        const auto &name = identifier.value.name;
        if (!typeInfo.contains(name)) {
            std::cerr << "Type of " << name << " cannot be determined." << std::endl;
            return {};
        }
        return type::Type{typeInfo.at(name)};
    }

    if (std::holds_alternative<AstNodeFuncCall>(expr)) {
        const auto &funcCall = std::get<AstNodeFuncCall>(expr);
        std::cerr << "Cannot determine the return type of " << funcCall.functionName.name << std::endl;
        return {};
    }

    if (auto const binaryOp = to<AstNodeBinaryOp>(expr)) {
        const auto lhsType = getExpressionType(binaryOp->operands[0], typeInfo, funcDefs);
        const auto rhsType = getExpressionType(binaryOp->operands[1], typeInfo, funcDefs);
        if (!lhsType || !rhsType) {
            std::cerr << "Couldn't determine the type of one of the operands." << std::endl;
            return {};
        }

        return getBinaryOperationType(binaryOp->op, *lhsType, *rhsType);
    }

    if (std::holds_alternative<AstNodeNegation>(expr)) {
        const auto &negation = std::get<AstNodeNegation>(expr);
        const auto type = getExpressionType(negation.operands[0], typeInfo, funcDefs);
        if (!type || !std::holds_alternative<type::I64>(*type)) {
            std::cerr << "Type of the expression is not integer." << std::endl;
            return {};
        }
        return type::I64{};
    }

    if (const auto &fnRef = to<AstNodeFuncRef>(expr)) {
        auto it = funcDefs.find(fnRef->generatedName);
        assert(it != funcDefs.end());
        const auto &fnDef = it->second;

        auto argTypes = std::vector<type::Type>{};
        for (const auto &[argName, argType] : fnDef.arguments) {
            if (auto type = getTypeFromName(argType.name)) {
                argTypes.push_back(*type);
            } else {
                std::cerr << "Couldn't determine types of arguments." << std::endl;
                return {};
            }
        }

        auto retValTypes = std::vector<type::Type>{};
        for (const auto &[retValName, retValType] : fnDef.returnVals) {
            if (auto type = getTypeFromName(retValType.name)) {
                retValTypes.push_back(*type);
            } else {
                std::cerr << "Couldn't determine types of return values." << std::endl;
                return {};
            }
        }

        return type::Function{.inputTypes = std::move(argTypes), .returnTypes = std::move(retValTypes)};
    }

    std::cerr << "Unexpected expression type!" << std::endl;
    assert(false);
}

} // namespace

std::string printType(const type::Type &type) {
    if (std::holds_alternative<type::I64>(type)) {
        return "i64";
    } else if (std::holds_alternative<type::Bool>(type)) {
        return "bool";
    } else if (std::holds_alternative<type::String>(type)) {
        return "string";
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
    }

    std::cerr << "Unexpected type, index: " << type.index() << std::endl;
    assert(false);
}

TypeInfo resolveTypes(const ParserOutput &parserOutput) {
    auto types = TypeInfo{};

    for (const auto &stmt : parserOutput.ast) {
        if (is<AstNodeDeclaration>(stmt)) {
            auto const &decl = as<AstNodeDeclaration>(stmt);
            auto const &name = decl.variable.name;

            auto type = getTypeFromName(decl.type.name);
            if (!type) {
                std::cerr << "Can't determine the type of " << name << std::endl;
                continue;
            }

            types.insert_or_assign(name, *type);

        } else if (std::holds_alternative<AstNodeAssignment>(stmt)) {
            auto assignment = std::get<AstNodeAssignment>(stmt);
            const auto &name = assignment.variable.name;

            auto type = getExpressionType(assignment.value, types, parserOutput.functions);
            if (!type) {
                std::cerr << "Can't determine the type of " << name << std::endl;
                continue;
            }

            if (types.contains(name) && types.at(name) != *type) {
                std::cerr << name << " redeclared with a different type (" << printType(types.at(name)) << " vs "
                          << printType(*type) << ")" << std::endl;
                continue;
            }

            types.insert_or_assign(name, *type);
        }
    }

    return types;
}
