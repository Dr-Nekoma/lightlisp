#include "prepare.h"

void prepareArena(CodegenContext &codegenContext) {
  auto &builder = codegenContext.builder();
  auto i8Ptr = llvm::PointerType::get(
      llvm::IntegerType::get(codegenContext.context(), 8), 0);
  auto sizeVal = builder.CreateLoad(builder.getInt64Ty(),
                                    codegenContext.getArenaSizeGV(), "size");

  int PROT_READ = 1;
  int PROT_WRITE = 2;
  int MAP_PRIVATE = 2;
  int MAP_ANON = 0x20;
  // constants for prot/flags
  auto prot = builder.getInt32(PROT_READ | PROT_WRITE);
  auto flags = builder.getInt32(MAP_ANON | MAP_PRIVATE);
  auto fd = builder.getInt32(-1);
  auto off = builder.getInt64(0);

  // call mmap(NULL, size, prot, flags, fd, off)
  auto basePtr = builder.CreateCall(
      codegenContext.getmmapFn(),
      {llvm::Constant::getNullValue(i8Ptr), sizeVal, prot, flags, fd, off},
      "arenaBaseRaw");
  // store into your global
  builder.CreateStore(basePtr, codegenContext.getArenaPtrGV());
}

llvm::Value *prepareCMain(CodegenContext &codegenContext,
                          llvm::Function *lispMain) {
  auto &builder = codegenContext.builder();
  auto boxedRet = builder.CreateCall(lispMain, {}, "boxedRet");
  llvm::Function *unboxFn = codegenContext.getFn("unboxInt");
  if (!unboxFn)
    throw std::runtime_error("unboxInt not declared");
  llvm::Value *retI64 = builder.CreateCall(unboxFn, {boxedRet}, "unboxedVal");

  auto ret32 = builder.CreateTrunc(retI64, builder.getInt32Ty(), "ret32");
  return ret32;
}

void munmapArena(CodegenContext &codegenContext) {
  auto &builder = codegenContext.builder();
  auto i8Ptr = llvm::PointerType::get(
      llvm::IntegerType::get(codegenContext.context(), 8), 0);

  auto base = builder.CreateLoad(i8Ptr, codegenContext.getArenaPtrGV(), "base");
  auto sizeVal2 = builder.CreateLoad(builder.getInt64Ty(),
                                     codegenContext.getArenaSizeGV(), "size2");
  builder.CreateCall(codegenContext.getmunmapFn(), {base, sizeVal2});
}

llvm::Function *emitBuiltIn(CodegenContext &codegenContext,
                            std::string &&fnName, IntOpFn opFn) {
  auto &builder = codegenContext.builder();
  auto ptrTy = codegenContext.getPtrType();
  // -- declare: Value* @fnName(Value*,Value*)
  llvm::FunctionType *FT =
      llvm::FunctionType::get(ptrTy, {ptrTy, ptrTy}, /*vararg=*/false);
  llvm::Function *F = llvm::Function::Create(
      FT, llvm::Function::ExternalLinkage, fnName, codegenContext.module());
  F->addFnAttr(llvm::Attribute::AlwaysInline);

  // name args %x0 %x1
  unsigned idx = 0;
  for (auto &arg : F->args())
    arg.setName("x" + std::to_string(idx++));

  // entry block + store args into allocas
  llvm::BasicBlock *BB =
      llvm::BasicBlock::Create(codegenContext.context(), "entry", F);
  builder.SetInsertPoint(BB);
  codegenContext.named_values().clear();
  for (auto &arg : F->args()) {
    auto name = std::string(arg.getName());
    auto *slot = CreateEntryBlockAlloca(codegenContext, F, ptrTy, name);
    builder.CreateStore(&arg, slot);
    codegenContext.named_values()[name] = slot;
  }

  // unbox both arguments
  llvm::Function *unboxFn = codegenContext.getFn("unboxInt");
  llvm::Value *lhs =
      builder.CreateCall(unboxFn, {&*F->args().begin()}, "unboxedLHS");
  llvm::Value *rhs = builder.CreateCall(
      unboxFn, {&*std::next(F->args().begin())}, "unboxedRHS");

  // apply the integer op
  llvm::Value *resI64 = opFn(builder, lhs, rhs);

  llvm::Function *boxFn = codegenContext.getFn("boxInt");
  auto boxed = builder.CreateCall(boxFn, {resI64}, "ret." + fnName);
  // return it
  builder.CreateRet(boxed);
  return F;
}

llvm::Function *emitPanic(CodegenContext &codegenContext) {
  auto voidType = llvm::Type::getVoidTy(codegenContext.context());

  llvm::FunctionType *FT =
      llvm::FunctionType::get(voidType, {}, /*vararg=*/false);
  llvm::Function *F = llvm::Function::Create(
      FT, llvm::Function::InternalLinkage, "panic", codegenContext.module());

  // entry block + store args into allocas
  llvm::BasicBlock *BB =
      llvm::BasicBlock::Create(codegenContext.context(), "entry", F);
  llvm::IRBuilder<> builder(BB);

  auto i8Ptr = llvm::PointerType::get(
      llvm::IntegerType::get(codegenContext.context(), 8), 0);

  auto base = builder.CreateLoad(i8Ptr, codegenContext.getArenaPtrGV(), "base");
  auto sizeVal = builder.CreateLoad(builder.getInt64Ty(),
                                    codegenContext.getArenaSizeGV(), "size");
  builder.CreateCall(codegenContext.getmunmapFn(), {base, sizeVal});
  builder.CreateCall(codegenContext.getTrapFn(), {});
  builder.CreateUnreachable();
  return F;
}

llvm::Function *emitBoxInt(CodegenContext &codegenContext) {
  llvm::StructType *ValueTy = codegenContext.getValueTy();
  auto &builder = codegenContext.builder();
  auto i64Ty = builder.getInt64Ty();

  llvm::FunctionType *FT = llvm::FunctionType::get(codegenContext.getPtrType(),
                                                   {i64Ty}, /*vararg=*/false);
  llvm::Function *F = llvm::Function::Create(
      FT, llvm::Function::InternalLinkage, "boxInt", codegenContext.module());

  F->addFnAttr(llvm::Attribute::AlwaysInline);
  F->arg_begin()->setName("x");

  llvm::BasicBlock *BB =
      llvm::BasicBlock::Create(codegenContext.context(), "entry", F);

  builder.SetInsertPoint(BB);

  auto &arg = *F->arg_begin(); // x0

  auto boxed =
      builder.CreateCall(codegenContext.getArenaAllocator(), {}, "boxedVal");

  auto intDescGV = codegenContext.module().getNamedGlobal("type.Int");
  auto typeGEP = builder.CreateStructGEP(ValueTy, boxed, 0, "type.ptr");
  builder.CreateStore(intDescGV, typeGEP);

  auto payloadGEP = builder.CreateStructGEP(ValueTy, boxed, 1, "payload.ptr");
  auto i64Ptr = builder.CreateBitCast(
      payloadGEP, llvm::PointerType::get(i64Ty, 0), "payload.i64.ptr");
  builder.CreateStore(&arg, i64Ptr);
  builder.CreateRet(boxed);
  return F;
}

llvm::Function *emitUnBoxInt(CodegenContext &codegenContext) {
  llvm::StructType *valueTy = codegenContext.getValueTy();
  auto &builder = codegenContext.builder();
  auto i64Ty = builder.getInt64Ty();
  auto i32Ty = builder.getInt32Ty();

  llvm::FunctionType *FT = llvm::FunctionType::get(
      i64Ty, {codegenContext.getPtrType()}, /*vararg=*/false);
  llvm::Function *F = llvm::Function::Create(
      FT, llvm::Function::InternalLinkage, "unboxInt", codegenContext.module());

  F->addFnAttr(llvm::Attribute::AlwaysInline);
  F->arg_begin()->setName("x");

  llvm::BasicBlock *BB =
      llvm::BasicBlock::Create(codegenContext.context(), "entry", F);

  builder.SetInsertPoint(BB);

  auto &arg = *F->arg_begin(); // x0

  auto tdGEP = builder.CreateStructGEP(valueTy, &arg, 0, "type.ptr");
  auto tdPtr =
      builder.CreateLoad(codegenContext.getTypeDescTy()->getPointerTo(), tdGEP,
                         "type.description");

  auto kindPtr = builder.CreateStructGEP(codegenContext.getTypeDescTy(), tdPtr,
                                         1, "kind.ptr");
  auto kind = builder.CreateLoad(i32Ty, kindPtr, "kind");

  auto isInt = builder.CreateICmpEQ(
      kind, llvm::ConstantInt::get(i32Ty, /*Int kind=*/0), "cmp.isIntKind");

  auto contBB = llvm::BasicBlock::Create(codegenContext.context(), "unbox.ok",
                                         BB->getParent());

  auto panicBB = llvm::BasicBlock::Create(codegenContext.context(),
                                          "unbox.error", BB->getParent());

  builder.CreateCondBr(isInt, contBB, panicBB);

  builder.SetInsertPoint(panicBB);
  llvm::Function *panicFn = codegenContext.getFn("panic");

  builder.CreateCall(panicFn, {});

  builder.CreateUnreachable();

  builder.SetInsertPoint(contBB);

  llvm::Value *valPayloadGEP =
      builder.CreateStructGEP(valueTy, &arg, 1, "val.payload.ptr");
  llvm::Value *valI64Ptr = builder.CreateBitCast(
      valPayloadGEP, builder.getInt64Ty()->getPointerTo(), "val.i64.ptr");
  llvm::Value *valI64 =
      builder.CreateLoad(builder.getInt64Ty(), valI64Ptr, "val.unboxed");
  builder.CreateRet(valI64);

  return F;
}