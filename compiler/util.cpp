#include "util.h"

VarInst::VarInst(llvm::AllocaInst *alloca) : local_(alloca), global_(nullptr) {}

VarInst::VarInst(llvm::GlobalVariable *global)
    : local_(nullptr), global_(global) {}

[[nodiscard]] llvm::AllocaInst *VarInst::get() const { return local_; }

[[nodiscard]] llvm::GlobalVariable *VarInst::getGlob() const { return global_; }

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