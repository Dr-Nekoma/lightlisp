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

  auto tdDesc = loadValType(val);

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

  builder.CreateCall(panicFn, {kind, builder.getInt32(toKind(type))});

  builder.CreateUnreachable();

  builder.SetInsertPoint(contBB);
}

llvm::Value *CodegenContext::TypeRegistry::unpackVal(llvm::Value *val,
                                                     BuiltInType type) {
  auto valPayloadGEP = getValPL(val);
  auto unpackedVal = parent_->context.builder.CreateLoad(
      toLLVMType(type), valPayloadGEP, "val.unboxed");
  return unpackedVal;
}

llvm::Value *CodegenContext::TypeRegistry::packVal(llvm::Value *val,
                                                   BuiltInType type) {
  auto boxed = makeBox();

  auto typeDescGV = parent_->type_manager.getType(type);
  storeValType(typeDescGV, boxed);
  storeValPL(val, boxed);
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
  // types[0] = envType;
  return llvm::FunctionType::get(ptrType, types, false);
}

llvm::Value *CodegenContext::TypeRegistry::makeBox() {
  uint64_t typeSize =
      parent_->context.module.getDataLayout().getTypeAllocSize(valueType);

  return parent_->context.builder.CreateCall(
      parent_->memory_manager.getArenaAllocator(),
      {parent_->context.builder.getInt64(typeSize)}, "boxedVal");
}

llvm::Value *CodegenContext::TypeRegistry::getValType(llvm::Value *val) {
  return parent_->context.builder.CreateStructGEP(valueType, val, 0,
                                                  "val.type.ptr");
}

llvm::Value *CodegenContext::TypeRegistry::getValPL(llvm::Value *val) {
  return parent_->context.builder.CreateStructGEP(valueType, val, 1,
                                                  "val.payload.ptr");
}

llvm::Value *CodegenContext::TypeRegistry::getEnvSize(llvm::Value *env) {
  return parent_->context.builder.CreateStructGEP(envType, env, 0,
                                                  "env.size.ptr");
}

llvm::Value *CodegenContext::TypeRegistry::getEnvStorage(llvm::Value *env) {
  return parent_->context.builder.CreateStructGEP(envType, env, 1,
                                                  "env.slots.ptr");
}

llvm::Value *CodegenContext::TypeRegistry::getClosureEnv(llvm::Value *closure) {
  return parent_->context.builder.CreateStructGEP(closureType, closure, 0,
                                                  "closure.env.ptr");
}

llvm::Value *CodegenContext::TypeRegistry::getClosureFn(llvm::Value *closure) {
  return parent_->context.builder.CreateStructGEP(closureType, closure, 1,
                                                  "closure.fn.ptr");
}

llvm::Value *
CodegenContext::TypeRegistry::getClosureSize(llvm::Value *closure) {
  return parent_->context.builder.CreateStructGEP(closureType, closure, 2,
                                                  "closure.size.ptr");
}

llvm::Value *CodegenContext::TypeRegistry::loadValType(llvm::Value *val) {
  return parent_->context.builder.CreateLoad(ptrType, getValType(val),
                                             "val.type");
}

llvm::Value *CodegenContext::TypeRegistry::loadEnvSize(llvm::Value *env) {
  return parent_->context.builder.CreateLoad(i64Type, getEnvSize(env),
                                             "env.size");
}

llvm::Value *CodegenContext::TypeRegistry::loadEnvStorage(llvm::Value *env) {
  return parent_->context.builder.CreateLoad(ptrType, getEnvStorage(env),
                                             "env.storage");
}

llvm::Value *
CodegenContext::TypeRegistry::loadClosureEnv(llvm::Value *closure) {
  return parent_->context.builder.CreateLoad(envType, getClosureEnv(closure),
                                             "closure.env");
}

llvm::Value *CodegenContext::TypeRegistry::loadClosureFn(llvm::Value *closure) {
  return parent_->context.builder.CreateLoad(ptrType, getClosureFn(closure),
                                             "closure.fn");
}

llvm::Value *
CodegenContext::TypeRegistry::loadClosureSize(llvm::Value *closure) {
  return parent_->context.builder.CreateLoad(i32Type, getClosureSize(closure),
                                             "closure.size");
}

llvm::Value *CodegenContext::TypeRegistry::storeValType(llvm::Value *type,
                                                        llvm::Value *val) {
  return parent_->context.builder.CreateStore(type, getValType(val));
}

llvm::Value *CodegenContext::TypeRegistry::storeValPL(llvm::Value *payload,
                                                      llvm::Value *val) {
  return parent_->context.builder.CreateStore(payload, getValPL(val));
}

llvm::Value *CodegenContext::TypeRegistry::storeEnvSize(llvm::Value *size,
                                                        llvm::Value *env) {
  return parent_->context.builder.CreateStore(size, getEnvSize(env));
}

llvm::Value *CodegenContext::TypeRegistry::storeEnvStorage(llvm::Value *storage,
                                                           llvm::Value *env) {
  return parent_->context.builder.CreateStore(storage, getEnvStorage(env));
}

llvm::Value *
CodegenContext::TypeRegistry::storeClosureEnv(llvm::Value *env,
                                              llvm::Value *closure) {
  return parent_->context.builder.CreateStore(env, getClosureEnv(closure));
}

llvm::Value *
CodegenContext::TypeRegistry::storeClosureFn(llvm::Function *fn,
                                             llvm::Value *closure) {
  return parent_->context.builder.CreateStore(fn, getClosureFn(closure));
}

llvm::Value *
CodegenContext::TypeRegistry::storeClosureSize(llvm::Value *size,
                                               llvm::Value *closure) {
  return parent_->context.builder.CreateStore(size, getClosureSize(closure));
}