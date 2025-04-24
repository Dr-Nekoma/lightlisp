#pragma once
#include "meta.h"

class CodegenContext;

llvm::StructType *makeTypeDescType(CodegenContext &codegenContext);

llvm::StructType *makeValueType(CodegenContext &codegenContext,
                                llvm::StructType *TypeDescTy);

void createBuiltinTypeDescs(CodegenContext &codegenContext);