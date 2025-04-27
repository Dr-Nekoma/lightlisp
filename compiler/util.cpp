#include "util.h"

llvm::AllocaInst *CreateEntryBlockAlloca(CodegenContext &codegenContext,
                                         llvm::Function *currentFn,
                                         llvm::Type *type,
                                         const std::string &VarName) {
  llvm::IRBuilder<> TmpB(&currentFn->getEntryBlock(),
                         currentFn->getEntryBlock().begin());
  return TmpB.CreateAlloca(type, nullptr, VarName);
}

std::string getBuiltInName(std::string &&name) {
  return "__internal_op_" + std::string(name);
}