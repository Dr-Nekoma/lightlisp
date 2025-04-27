#pragma once
#include "Optimizer.h"
#include "ir1lisp.h"
#include "meta.h"
#include "objects.h"
#include "types.h"
#include "util.h"

void prepareArena(CodegenContext &codegenContext);

llvm::Value *prepareCMain(CodegenContext &codegenContext,
                          llvm::Function *lispMain);

void munmapArena(CodegenContext &codegenContext);

llvm::Function *emitBuiltIn(CodegenContext &codegenContext,
                            std::string &&fnName, IntOpFn opFn);

llvm::Function *emitPanic(CodegenContext &codegenContext);

llvm::Function *emitBoxInt(CodegenContext &codegenContext);

llvm::Function *emitUnBoxInt(CodegenContext &codegenContext);