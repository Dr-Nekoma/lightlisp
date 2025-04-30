#pragma once
#include "meta.h"
#include "objects.h"

llvm::AllocaInst *CreateEntryBlockAlloca(CodegenContext &codegenContext,
                                         llvm::Function *currentFn,
                                         llvm::Type *type,
                                         const std::string &VarName);

std::string getBuiltInName(std::string &&name);