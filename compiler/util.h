#pragma once
#include "meta.h"
#include "objects.h"
#include "types.h"

llvm::AllocaInst *CreateEntryBlockAlloca(CodegenContext &codegenContext,
                                         llvm::Function *currentFn,
                                         llvm::Type *type,
                                         const std::string &VarName);

llvm::CallInst *boxIntVal(CodegenContext &codegenContext, llvm::Value *val,
                          std::string &&name);

llvm::Value *unboxIntVal(CodegenContext &codegenContext, llvm::Value *val);

std::string getBuiltInName(std::string &&name);