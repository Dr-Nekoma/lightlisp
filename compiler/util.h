#pragma once
#include "llvmincludes.h"
#include <variant>

struct VarInst {
public:
  VarInst(llvm::AllocaInst *alloca);

  VarInst(llvm::GlobalVariable *global);

  [[nodiscard]] llvm::AllocaInst *get() const;

  [[nodiscard]] llvm::GlobalVariable *getGlob() const;

private:
  llvm::AllocaInst *local_;
  llvm::GlobalVariable *global_;
};

llvm::AllocaInst *CreateEntryBlockAlloca(llvm::Function *currentFn,
                                         llvm::Type *type,
                                         const std::string &VarName);

std::string getBuiltInName(std::string &&name);

std::string getGlobalFnName(std::string &&name);
