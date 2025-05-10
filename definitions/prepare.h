#pragma once
#include "Optimizer.h"
#include "ir1lisp.h"
#include "meta.h"
#include "objects.h"
#include "util.h"

llvm::Value *prepareCMain(CodegenContext &codegenContext);

template <unsigned Arity, typename Op>
llvm::Function *emitBuiltIn(CodegenContext &codegenContext,
                            llvm::StringRef fnName, Op opFn,
                            llvm::Function *unboxFn, llvm::Function *boxFn) {
  auto &M = codegenContext.context.module;
  auto &C = codegenContext.context.context;
  auto ptrTy = codegenContext.type_manager.getPtrType();
  // Build FunctionType: (ptr,ptr,… Arity times) -> ptr
  llvm::SmallVector<llvm::Type *, Arity> params(Arity, ptrTy);
  auto FT = llvm::FunctionType::get(ptrTy, params, /*isVarArg=*/false);

  auto F =
      llvm::Function::Create(FT, llvm::Function::ExternalLinkage, fnName, M);
  F->addFnAttr(llvm::Attribute::AlwaysInline);

  unsigned idx = 0;
  for (auto &arg : F->args())
    arg.setName(("x" + std::to_string(idx++)));

  auto entry = llvm::BasicBlock::Create(C, "entry", F);
  llvm::IRBuilder<> B(entry);

  llvm::SmallVector<llvm::Value *, Arity> rawArgs;
  idx = 0;
  for (auto &arg : F->args()) {
    if (unboxFn) {
      rawArgs.push_back(
          B.CreateCall(unboxFn, {&arg}, ("u" + std::to_string(idx++))));
    } else {
      rawArgs.push_back(&arg);
    }
  }

  llvm::Value *rawRes = opFn(B, rawArgs);

  if (boxFn) {
    llvm::Value *boxed = B.CreateCall(boxFn, {rawRes}, ("res." + fnName).str());
    B.CreateRet(boxed);
  } else {
    B.CreateRet(rawRes);
  }

  return F;
}

llvm::Function *emitPanic(CodegenContext &codegenContext);

llvm::Function *emitBoxInt(CodegenContext &codegenContext);

llvm::Function *emitUnBoxInt(CodegenContext &codegenContext);

llvm::Function *emitCons(CodegenContext &codegenContext);

llvm::Function *emitCar(CodegenContext &codegenContext);

llvm::Function *emitCdr(CodegenContext &codegenContext);

llvm::Function *emitBoxCons(CodegenContext &codegenContext);

llvm::Function *emitUnBoxCons(CodegenContext &codegenContext);

llvm::Function *emitUnBoxFn(CodegenContext &codegenContext);