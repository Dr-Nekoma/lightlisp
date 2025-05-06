#include "meta.h"

CodegenContext::TypeRegistry::TypeRegistry(IRGenContext &irgc)
    : typeDescTy_(makeTypeDescType(irgc)),
      valueTy_(makeValueType(irgc, typeDescTy_)),
      ptrTy_(llvm::PointerType::get(irgc.context, 0)),
      consTy_(makeConsType(irgc)), envTy_(makeEnvType(irgc)),
      closureTy_(makeClosureType(irgc)) {

  createBuiltinTypeDescVar(irgc, "Fn", -1);
  createBuiltinTypeDescVar(irgc, "Int", 0);
  createBuiltinTypeDescVar(irgc, "Cons", 1);
}

llvm::StructType *
CodegenContext::TypeRegistry::makeTypeDescType(IRGenContext &irgc) {
  auto i8Ptr =
      llvm::PointerType::get(llvm::IntegerType::get(irgc.context, 8), 0);
  auto i32Ty = llvm::Type::getInt32Ty(irgc.context);
  auto TD = llvm::StructType::create(irgc.context, "TypeDesc");
  TD->setBody({i8Ptr, i32Ty}, /*isPacked=*/false);
  return TD;
}

llvm::StructType *
CodegenContext::TypeRegistry::makeValueType(IRGenContext &irgc,
                                            llvm::StructType *TypeDescTy) {
  auto TDPtr = llvm::PointerType::get(TypeDescTy, 0);
  auto payload = llvm::ArrayType::get(llvm::Type::getInt8Ty(irgc.context), 8);
  auto ValTy = llvm::StructType::create(irgc.context, "Value");
  ValTy->setBody({TDPtr, payload}, /*isPacked=*/false);
  return ValTy;
}

llvm::StructType *
CodegenContext::TypeRegistry::makeConsType(IRGenContext &irgc) {
  auto valueTy = getValueTy();
  return llvm::StructType::create(irgc.context, {valueTy, valueTy}, "Cons");
}

llvm::StructType *
CodegenContext::TypeRegistry::makeEnvType(IRGenContext &irgc) {
  auto &context = irgc.context;

  auto envTy = llvm::StructType::create(context, "Env");
  // auto envPtr = llvm::PointerType::getUnqual(envTy);
  auto i64Ty = llvm::Type::getInt64Ty(context);

  envTy->setBody({i64Ty, llvm::PointerType::getUnqual(getPtrType())}, false);

  return envTy;
}

llvm::StructType *
CodegenContext::TypeRegistry::makeClosureType(IRGenContext &irgc) {
  auto &context = irgc.context;

  auto ptr = llvm::PointerType::get(context, 0);
  auto i32Ty = llvm::Type::getInt32Ty(context);

  auto closureTy = llvm::StructType::create(context, "Closure");

  closureTy->setBody({ptr, ptr, i32Ty});
  return closureTy;
}

llvm::GlobalVariable *CodegenContext::TypeRegistry::createBuiltinTypeDescVar(
    IRGenContext &irgc, llvm::StringRef name, int kind) {
  auto TypeDescTy = getTypeDescTy();
  auto i32Ty = llvm::Type::getInt32Ty(irgc.context);

  // name string constant
  auto nameConst = llvm::ConstantDataArray::getString(irgc.context, name);
  auto nameGV = new llvm::GlobalVariable(
      irgc.module, nameConst->getType(), true,
      llvm::GlobalValue::PrivateLinkage, nameConst, name + ".str");
  // ptr to name
  auto zero = llvm::ConstantInt::get(i32Ty, 0);
  llvm::SmallVector<llvm::Constant *, 2> idxs = {zero, zero};
  auto namePtr = llvm::ConstantExpr::getInBoundsGetElementPtr(
      nameConst->getType(), nameGV, idxs);
  // struct initializer { i8* namePtr, i32 kind }
  auto init = llvm::ConstantStruct::get(
      TypeDescTy, {namePtr, llvm::ConstantInt::get(i32Ty, kind)});
  // the global TypeDesc
  auto newtype = new llvm::GlobalVariable(irgc.module, TypeDescTy, true,
                                          llvm::GlobalValue::ExternalLinkage,
                                          init, "type." + name);
  builtInTypes_.emplace(std::string(name), newtype);
  return newtype;
}

llvm::GlobalVariable *
CodegenContext::TypeRegistry::getType(const std::string &name) {
  if (auto it = builtInTypes_.find(name); it != builtInTypes_.end()) {
    return it->second;
  }
  return nullptr;
}

llvm::StructType *CodegenContext::TypeRegistry::getConsTy() { return consTy_; }

llvm::StructType *CodegenContext::TypeRegistry::getTypeDescTy() {
  return typeDescTy_;
}

llvm::StructType *CodegenContext::TypeRegistry::getValueTy() {
  return valueTy_;
}

llvm::PointerType *CodegenContext::TypeRegistry::getPtrType() { return ptrTy_; }

llvm::StructType *CodegenContext::TypeRegistry::getEnvTy() { return envTy_; }

llvm::StructType *CodegenContext::TypeRegistry::getClosureTy() {
  return closureTy_;
}
