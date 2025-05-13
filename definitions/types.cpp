#include "meta.h"

CodegenContext::TypeRegistry::TypeRegistry(CodegenContext &context)
    : ptrType(llvm::PointerType::get(context.context.context, 0)),
      i32Type(llvm::Type::getInt32Ty(context.context.context)),
      i64Type(llvm::Type::getInt64Ty(context.context.context)),
      typeDescType(llvm::StructType::create(context.context.context,
                                            {ptrType, i32Type}, "TypeDesc")),
      valueType(makeValueType(context.context)),
      consType(llvm::StructType::create(context.context.context,
                                        {ptrType, ptrType}, "Cons")),
      envType(llvm::StructType::create(context.context.context,
                                       {i64Type, ptrType}, "Env")),
      closureType(llvm::StructType::create(
          context.context.context, {envType, ptrType, i32Type}, "Closure")),
      parent_(&context) {

  createBuiltinTypeDescVar(Type::Fn);
  createBuiltinTypeDescVar(Type::Int);
  createBuiltinTypeDescVar(Type::Cons);
}

llvm::StructType *
CodegenContext::TypeRegistry::makeValueType(IRGenContext &irgc) {
  auto payload = llvm::ArrayType::get(llvm::Type::getInt8Ty(irgc.context), 8);
  auto ValTy =
      llvm::StructType::create(irgc.context, {ptrType, payload}, "Value");
  return ValTy;
}

llvm::GlobalVariable *
CodegenContext::TypeRegistry::createBuiltinTypeDescVar(BuiltInType type) {
  auto name = toStrName(type);
  auto nameConst =
      llvm::ConstantDataArray::getString(parent_->context.context, name);
  auto nameGV = new llvm::GlobalVariable(
      parent_->context.module, nameConst->getType(), true,
      llvm::GlobalValue::PrivateLinkage, nameConst, name + ".str");
  // ptr to name
  auto zero = llvm::ConstantInt::get(i32Type, 0);
  llvm::SmallVector<llvm::Constant *, 2> idxs = {zero, zero};
  auto namePtr = llvm::ConstantExpr::getInBoundsGetElementPtr(
      nameConst->getType(), nameGV, idxs);
  // struct initializer { i8* namePtr, i32 kind }
  auto init = llvm::ConstantStruct::get(
      typeDescType, {namePtr, llvm::ConstantInt::get(i32Type, toKind(type))});
  // the global TypeDesc
  auto newtype = new llvm::GlobalVariable(
      parent_->context.module, typeDescType, true,
      llvm::GlobalValue::ExternalLinkage, init, "type." + name);
  builtInTypes_.emplace(type, newtype);
  return newtype;
}

llvm::GlobalVariable *CodegenContext::TypeRegistry::getType(BuiltInType type) {
  if (auto it = builtInTypes_.find(type); it != builtInTypes_.end()) {
    return it->second;
  }
  return nullptr;
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

void CodegenContext::TypeRegistry::emitCheckType(llvm::Value *val,
                                                 BuiltInType type) {
  auto &builder = parent_->context.builder;

  auto tdGEP = builder.CreateStructGEP(valueType, val, 0, "type.ptr");
  auto tdDesc = builder.CreateLoad(ptrType, tdGEP, "type.desc");

  auto kindPtr = builder.CreateStructGEP(typeDescType, tdDesc, 1, "kind.ptr");
  auto kind = builder.CreateLoad(i32Type, kindPtr, "kind");

  auto isInt =
      builder.CreateICmpEQ(kind, llvm::ConstantInt::get(i32Type, toKind(type)),
                           "cmp.is" + toStrName(type) + "Kind");

  auto curBB = builder.GetInsertBlock();

  auto contBB = llvm::BasicBlock::Create(parent_->context.context, "unbox.ok",
                                         curBB->getParent());

  auto panicBB = llvm::BasicBlock::Create(parent_->context.context,
                                          "unbox.error", curBB->getParent());

  builder.CreateCondBr(isInt, contBB, panicBB);

  builder.SetInsertPoint(panicBB);
  llvm::Function *panicFn = parent_->lexenv.getBuiltInFn("panic");

  builder.CreateCall(panicFn, {});

  builder.CreateUnreachable();

  builder.SetInsertPoint(contBB);
}

void CodegenContext::TypeRegistry::typeDebug(llvm::Value *val) {
  auto &builder = parent_->context.builder;

  auto tdGEP = builder.CreateStructGEP(valueType, val, 0, "type.ptr");
  auto tdDesc = builder.CreateLoad(ptrType, tdGEP, "type.desc");

  auto kindPtr = builder.CreateStructGEP(typeDescType, tdDesc, 1, "kind.ptr");
  auto kind = builder.CreateLoad(i32Type, kindPtr, "kind");
  builder.CreateStore(kind, parent_->debug);
}

llvm::Value *CodegenContext::TypeRegistry::unpackVal(llvm::Value *val,
                                                     BuiltInType type) {
  auto valPayloadGEP = parent_->context.builder.CreateStructGEP(
      valueType, val, 1, "val.payload.ptr");
  auto unpackedVal = parent_->context.builder.CreateLoad(
      toLLVMType(type), valPayloadGEP, "val.unboxed");
  return unpackedVal;
}

llvm::Value *CodegenContext::TypeRegistry::packVal(llvm::Value *val,
                                                   BuiltInType type) {

  auto &builder = parent_->context.builder;

  uint64_t typeSize =
      parent_->context.module.getDataLayout().getTypeAllocSize(valueType);

  auto boxed = builder.CreateCall(parent_->memory_manager.getArenaAllocator(),
                                  {builder.getInt64(typeSize)}, "boxedVal");

  auto typeDescGV = parent_->type_manager.getType(type);
  auto typeGEP = builder.CreateStructGEP(valueType, boxed, 0, "type.ptr");
  builder.CreateStore(typeDescGV, typeGEP);

  auto payloadGEP = builder.CreateStructGEP(valueType, boxed, 1, "payload.ptr");
  builder.CreateStore(val, payloadGEP);
  return boxed;
}

llvm::Value *CodegenContext::TypeRegistry::checkAndUnpack(llvm::Value *val,
                                                          BuiltInType type) {
  emitCheckType(val, type);
  return unpackVal(val, type);
}

llvm::Type *CodegenContext::TypeRegistry::toLLVMType(BuiltInType type) {
  switch (type) {
  case CodegenContext::TypeRegistry::BuiltInType::Int:
    return i64Type;
  default:
    return ptrType;
  }
}

llvm::FunctionType *CodegenContext::TypeRegistry::getStdFnType(size_t args) {
  std::vector<llvm::Type *> types(1 + args, ptrType);
  types[0] = envType;
  return llvm::FunctionType::get(ptrType, types, false);
}