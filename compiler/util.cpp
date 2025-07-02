#include "util.h"

VarInst::VarInst(llvm::AllocaInst *alloca) : val_(alloca) {}

VarInst::VarInst(llvm::GlobalVariable *global) : val_(global) {}

[[nodiscard]] llvm::AllocaInst *VarInst::getLocal() const {
  return std::get<llvm::AllocaInst *>(val_);
}

[[nodiscard]] llvm::GlobalVariable *VarInst::getGlob() const {
  return std::get<llvm::GlobalVariable *>(val_);
}

TaggedLLVMVal::TaggedLLVMVal() : val_(static_cast<llvm::Value *>(nullptr)) {}

TaggedLLVMVal::TaggedLLVMVal(llvm::Value *val) : val_(val) {}

TaggedLLVMVal::TaggedLLVMVal(llvm::Function *fn) : val_(fn) {}

TaggedLLVMVal::TaggedLLVMVal(llvm::CallInst *call) : val_(call) {}

TaggedLLVMVal::TaggedLLVMVal(llvm::PHINode *phi) : val_(phi) {}

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