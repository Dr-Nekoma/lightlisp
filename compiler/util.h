#pragma once
#include "llvmincludes.h"
#include <variant>

struct VarInst {
  VarInst(llvm::AllocaInst *alloca);

  VarInst(llvm::GlobalVariable *global);

  [[nodiscard]] llvm::AllocaInst *getLocal() const;

  [[nodiscard]] llvm::GlobalVariable *getGlob() const;

private:
  std::variant<llvm::AllocaInst *, llvm::GlobalVariable *> val_;
};

struct TaggedLLVMVal {
  TaggedLLVMVal();

  TaggedLLVMVal(llvm::Value *val);

  TaggedLLVMVal(llvm::Function *fn);

  [[nodiscard]] llvm::Value *get() const;

  [[nodiscard]] llvm::Function *getFn() const;

  [[nodiscard]] bool isFn() const;

private:
  std::variant<llvm::Value *, llvm::Function *> val_;
};

llvm::AllocaInst *CreateEntryBlockAlloca(llvm::Function *currentFn,
                                         llvm::Type *type,
                                         const std::string &VarName);

std::string getBuiltInName(std::string &&name);

std::string getGlobalFnName(std::string &&name);
