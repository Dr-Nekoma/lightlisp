#include "meta.h"

CodegenContext::CodegenContext()
    : context_(std::make_unique<llvm::LLVMContext>()),
      builder_(std::make_unique<llvm::IRBuilder<>>(*context_)),
      module_(std::make_unique<llvm::Module>("my lisp", *context_)),
      typeDescTy_(makeTypeDescType(*this)),
      valueTy_(makeValueType(*this, typeDescTy_)),
      ptrTy_(llvm::PointerType::getUnqual(valueTy_)) {}

// Getter methods
llvm::LLVMContext &CodegenContext::context() { return *context_; }

llvm::IRBuilder<> &CodegenContext::builder() { return *builder_; }

llvm::Module &CodegenContext::module() { return *module_; }

std::map<std::string, llvm::AllocaInst *> &CodegenContext::named_values() {
  return named_values_;
}

std::vector<std::unordered_map<std::string, llvm::BasicBlock *>> &
CodegenContext::tagEnvs() {
  return tagEnvs_;
}

llvm::StructType *CodegenContext::getTypeDescTy() { return typeDescTy_; }

llvm::StructType *CodegenContext::getValueTy() { return valueTy_; }

llvm::PointerType *CodegenContext::getPtrType() { return ptrTy_; }

std::unordered_map<std::string, llvm::BasicBlock *> &
CodegenContext::lastTagEnv() {
  if (tagEnvs_.empty())
    throw std::runtime_error("Go outside of tagbody");
  return tagEnvs_.back();
}
