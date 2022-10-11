#include "types.h"

#include <cassert>
#include <iostream>
#include <sstream>

#include "variant_helpers.h"

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
        for (auto const &[mName, mType] : strct.members) {
            out << "(" << mName << ": " << printType(mType) << ") ";
        }
        out << "))";

        return out.str();
    }

    std::cerr << "Unexpected type, index: " << type.index() << std::endl;
    assert(false);
}
