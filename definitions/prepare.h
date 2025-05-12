#pragma once
#include "Optimizer.h"
#include "ir1lisp.h"
#include "meta.h"
#include "objects.h"
#include "util.h"
#include <optional>

llvm::Value *prepareCMain(CodegenContext &codegenContext);

template <unsigned Arity, typename Op>
llvm::Function *emitBuiltIn(
    CodegenContext &codegenContext, llvm::StringRef fnName, Op opFn,
    std::array<std::optional<CodegenContext::TypeRegistry::BuiltInType>, Arity>
        inTypes,
    std::optional<CodegenContext::TypeRegistry::BuiltInType> retType) {
  auto &[context, builder, module] = codegenContext.context;

  auto ptrType = codegenContext.type_manager.ptrType;
  // Build FunctionType: (ptr,ptr,… Arity times) -> ptr
  llvm::SmallVector<llvm::Type *, Arity> params(Arity, ptrType);
  auto FT = llvm::FunctionType::get(ptrType, params, /*isVarArg=*/false);

  auto F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, fnName,
                                  module);
  F->addFnAttr(llvm::Attribute::AlwaysInline);

  unsigned idx = 0;
  for (auto &arg : F->args())
    arg.setName(("x" + std::to_string(idx++)));

  auto entry = llvm::BasicBlock::Create(context, "entry", F);
  builder.SetInsertPoint(entry);

  llvm::SmallVector<llvm::Value *, Arity> rawArgs;
  idx = 0;
  for (auto &arg : F->args()) {
    if (inTypes[idx]) {
      auto unpacked = codegenContext.type_manager.checkAndUnpack(
          &arg, inTypes[idx].value());
      rawArgs.push_back(unpacked);
    } else {
      rawArgs.push_back(&arg);
    }
  }

  llvm::Value *rawRes = opFn(builder, rawArgs);

  if (retType) {
    llvm::Value *boxed =
        codegenContext.type_manager.packVal(rawRes, retType.value());
    builder.CreateRet(boxed);
  } else {
    builder.CreateRet(rawRes);
  }

  return F;
}

llvm::Function *emitPanic(CodegenContext &codegenContext);

llvm::Function *emitCons(CodegenContext &codegenContext);

llvm::Function *emitCar(CodegenContext &codegenContext);

llvm::Function *emitCdr(CodegenContext &codegenContext);
