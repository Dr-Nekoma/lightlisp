#include "util.h"

VarInst::VarInst(llvm::AllocaInst *alloca) : val_(alloca) {}

VarInst::VarInst(llvm::GlobalVariable *global) : val_(global) {}

[[nodiscard]] llvm::AllocaInst *VarInst::get() const {
  if (std::holds_alternative<llvm::AllocaInst *>(val_))
    return std::get<llvm::AllocaInst *>(val_);
  return nullptr;
}

[[nodiscard]] llvm::GlobalVariable *VarInst::getGlob() const {
  if (std::holds_alternative<llvm::GlobalVariable *>(val_))
    return std::get<llvm::GlobalVariable *>(val_);
  return nullptr;
}

llvm::AllocaInst *CreateEntryBlockAlloca(llvm::Function *currentFn,
                                         llvm::Type *type,
                                         const std::string &VarName) {
  llvm::IRBuilder<> TmpB(&currentFn->getEntryBlock(),
                         currentFn->getEntryBlock().begin());
  return TmpB.CreateAlloca(type, nullptr, VarName);
}

std::string getBuiltInName(std::string &&name) {
  return "__internal_op_" + name;
}

std::string getGlobalFnName(std::string &&name) { return "__closure_" + name; }