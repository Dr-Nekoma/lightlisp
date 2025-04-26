#include "Optimizer.h"
#include "ir1lisp.h"
#include "meta.h"
#include "objects.h"
#include "parser.h"
#include "tokenizer.h"
#include "types.h"
#include "util.h"

std::vector<std::unique_ptr<Function>>
prepareTopLevelFns(CodegenContext &codegenContext, Parser &&parser);

void prepareArena(CodegenContext &codegenContext);

llvm::Value *prepareCMain(CodegenContext &codegenContext,
                          llvm::Function *lispMain);

void munmapArena(CodegenContext &codegenContext);

void emitBuiltIn(CodegenContext &codegenContext, std::string &&fnName,
                 IntOpFn opFn);

void emitPanic(CodegenContext &codegenContext);

void initBuiltIns(CodegenContext &codegenContext);