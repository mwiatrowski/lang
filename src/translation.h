#pragma once

#include "ir.h"
#include "parser.h"

auto astToIr(ProgramDescription const &programDescription) -> ProgramIr;
