#include "meta.h"

CodegenContext::TypeRegistry::TypeRegistry(IRGenContext &irgc)
    : typeDescTy_(makeTypeDescType(irgc)), valueTy_(makeValueType(irgc)),
      ptrTy_(llvm::PointerType::get(irgc.context, 0)),
      consTy_(makeConsType(irgc)), envTy_(makeEnvType(irgc)),
      closureTy_(makeClosureType(irgc)) {

  createBuiltinTypeDescVar(irgc, Type::Fn);
  createBuiltinTypeDescVar(irgc, Type::Int);
  createBuiltinTypeDescVar(irgc, Type::Cons);
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
CodegenContext::TypeRegistry::makeValueType(IRGenContext &irgc) {
  auto payload = llvm::ArrayType::get(llvm::Type::getInt8Ty(irgc.context), 8);
  auto ValTy = llvm::StructType::create(irgc.context, "Value");
  ValTy->setBody({llvm::PointerType::get(irgc.context, 0), payload},
                 /*isPacked=*/false);
  return ValTy;
}

llvm::StructType *
CodegenContext::TypeRegistry::makeConsType(IRGenContext &irgc) {
  auto ptrTy = getPtrType();
  return llvm::StructType::create(irgc.context, {ptrTy, ptrTy}, "Cons");
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
  auto envTy = getEnvTy();
  auto closureTy = llvm::StructType::create(context, "Closure");

  closureTy->setBody({envTy, ptr, i32Ty});
  return closureTy;
}

llvm::GlobalVariable *
CodegenContext::TypeRegistry::createBuiltinTypeDescVar(IRGenContext &irgc,
                                                       BuiltInType type) {
  auto TypeDescTy = getTypeDescTy();
  auto i32Ty = llvm::Type::getInt32Ty(irgc.context);

  // name string constant
  auto name = toStrName(type);
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
      TypeDescTy, {namePtr, llvm::ConstantInt::get(i32Ty, toKind(type))});
  // the global TypeDesc
  auto newtype = new llvm::GlobalVariable(irgc.module, TypeDescTy, true,
                                          llvm::GlobalValue::ExternalLinkage,
                                          init, "type." + name);
  builtInTypes_.emplace(type, newtype);
  return newtype;
}

llvm::GlobalVariable *CodegenContext::TypeRegistry::getType(BuiltInType type) {
  if (auto it = builtInTypes_.find(type); it != builtInTypes_.end()) {
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

int CodegenContext::TypeRegistry::toKind(BuiltInType type) {
  switch (type) {
  case CodegenContext::TypeRegistry::BuiltInType::Int:
    return 0;
  case CodegenContext::TypeRegistry::BuiltInType::Cons:
    return 1;
  case CodegenContext::TypeRegistry::BuiltInType::Fn:
    return -1;
  }
}

std::string &CodegenContext::TypeRegistry::toStrName(BuiltInType type) {
  switch (type) {
  case CodegenContext::TypeRegistry::BuiltInType::Int: {
    static std::string intName("Int");
    return intName;
  }
  case CodegenContext::TypeRegistry::BuiltInType::Cons: {
    static std::string consName("Cons");
    return consName;
  }
  case CodegenContext::TypeRegistry::BuiltInType::Fn: {
    static std::string fnName("Fn");
    return fnName;
  }
  }
}

void CodegenContext::TypeRegistry::emitCheckType(CodegenContext &codegenContext,
                                                 llvm::Value *val,
                                                 BuiltInType type) {
  llvm::StructType *valueTy = getValueTy();
  auto &builder = codegenContext.context.builder;
  auto i32Ty = builder.getInt32Ty();

  auto ptrTy = getPtrType();
  auto tdTy = getTypeDescTy();

  auto tdGEP = builder.CreateStructGEP(valueTy, val, 0, "type.ptr");
  auto tdDesc = builder.CreateLoad(ptrTy, tdGEP, "type.desc");

  auto kindPtr = builder.CreateStructGEP(tdTy, tdDesc, 1, "kind.ptr");
  auto kind = builder.CreateLoad(i32Ty, kindPtr, "kind");

  auto isInt =
      builder.CreateICmpEQ(kind, llvm::ConstantInt::get(i32Ty, toKind(type)),
                           "cmp.is" + toStrName(type) + "Kind");

  auto curBB = builder.GetInsertBlock();

  auto contBB = llvm::BasicBlock::Create(codegenContext.context.context,
                                         "unbox.ok", curBB->getParent());

  auto panicBB = llvm::BasicBlock::Create(codegenContext.context.context,
                                          "unbox.error", curBB->getParent());

  builder.CreateCondBr(isInt, contBB, panicBB);

  builder.SetInsertPoint(panicBB);
  llvm::Function *panicFn = codegenContext.lexenv.getBuiltInFn("panic");

  builder.CreateCall(panicFn, {});

  builder.CreateUnreachable();

  builder.SetInsertPoint(contBB);
}

void CodegenContext::TypeRegistry::typeDebug(CodegenContext &codegenContext,
                                             llvm::Value *val) {
  llvm::StructType *valueTy = getValueTy();
  auto &builder = codegenContext.context.builder;
  auto i32Ty = builder.getInt32Ty();

  auto ptrTy = getPtrType();
  auto tdTy = getTypeDescTy();

  auto tdGEP = builder.CreateStructGEP(valueTy, val, 0, "type.ptr");
  auto tdDesc = builder.CreateLoad(ptrTy, tdGEP, "type.desc");

  auto kindPtr = builder.CreateStructGEP(tdTy, tdDesc, 1, "kind.ptr");
  auto kind = builder.CreateLoad(i32Ty, kindPtr, "kind");
  builder.CreateStore(kind, codegenContext.debug);
}

llvm::Value *
CodegenContext::TypeRegistry::unpackVal(CodegenContext &codegenContext,
                                        llvm::Value *val, BuiltInType type) {
  auto valPayloadGEP = codegenContext.context.builder.CreateStructGEP(
      getValueTy(), val, 1, "val.payload.ptr");
  auto unpackedVal = codegenContext.context.builder.CreateLoad(
      toLLVMType(codegenContext, type), valPayloadGEP, "val.unboxed");
  unpackedVal->setAlignment(llvm::Align(8));
  return unpackedVal;
}

llvm::Value *
CodegenContext::TypeRegistry::packVal(CodegenContext &codegenContext,
                                      llvm::Value *val, BuiltInType type) {

  llvm::StructType *ValueTy = codegenContext.type_manager.getValueTy();
  auto &builder = codegenContext.context.builder;

  uint64_t typeSize =
      codegenContext.context.module.getDataLayout().getTypeAllocSize(
          codegenContext.type_manager.getValueTy());

  auto boxed =
      builder.CreateCall(codegenContext.memory_manager.getArenaAllocator(),
                         {builder.getInt64(typeSize)}, "boxedVal");

  auto typeDescGV = codegenContext.type_manager.getType(type);
  auto typeGEP = builder.CreateStructGEP(ValueTy, boxed, 0, "type.ptr");
  builder.CreateStore(typeDescGV, typeGEP);

  auto payloadGEP = builder.CreateStructGEP(ValueTy, boxed, 1, "payload.ptr");
  builder.CreateStore(val, payloadGEP)->setAlignment(llvm::Align(8));
  return boxed;
}

llvm::Value *CodegenContext::TypeRegistry::checkAndUnpack(
    CodegenContext &codegenContext, llvm::Value *val, BuiltInType type) {
  emitCheckType(codegenContext, val, type);
  return unpackVal(codegenContext, val, type);
}

llvm::Type *
CodegenContext::TypeRegistry::toLLVMType(CodegenContext &codegenContext,
                                         BuiltInType type) {
  switch (type) {
  case CodegenContext::TypeRegistry::BuiltInType::Int:
    return codegenContext.context.builder.getInt64Ty();
  default:
    return codegenContext.type_manager.getPtrType();
  }
}