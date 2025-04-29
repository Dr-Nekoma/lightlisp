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

  uint64_t typeSize = codegenContext.module().getDataLayout().getTypeAllocSize(
      codegenContext.getValueTy());

  auto rawBoxed = builder.CreateCall(codegenContext.getArenaAllocator(),
                                     {builder.getInt64(typeSize)}, "boxedVal");

  auto boxed =
      builder.CreateBitCast(rawBoxed, codegenContext.getPtrType(), "cellPtr");

  auto intDescGV = codegenContext.getType("Int");
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

llvm::Function *emitCons(CodegenContext &codegenContext) {
  auto valuePtrTy = codegenContext.getPtrType();
  auto consTy = codegenContext.getConsTy();
  auto consPtrTy = consTy->getPointerTo();
  auto &builder = codegenContext.builder();

  llvm::FunctionType *FT =
      llvm::FunctionType::get(consPtrTy, {valuePtrTy, valuePtrTy},
                              /*vararg=*/false);
  llvm::Function *F =
      llvm::Function::Create(FT, llvm::Function::InternalLinkage,
                             "__internal_op_cons", codegenContext.module());

  F->addFnAttr(llvm::Attribute::AlwaysInline);
  auto it = F->arg_begin();
  it->setName("car");
  ++it;
  it->setName("cdr");

  auto BB = llvm::BasicBlock::Create(codegenContext.context(), "entry", F);
  builder.SetInsertPoint(BB);

  uint64_t consTypeSize =
      codegenContext.module().getDataLayout().getTypeAllocSize(
          codegenContext.getConsTy());
  auto rawCell =
      builder.CreateCall(codegenContext.getArenaAllocator(),
                         {builder.getInt64(consTypeSize)}, "rawCons");

  auto consPtr = builder.CreateBitCast(rawCell, consPtrTy, "cons.ptr");

  it = F->arg_begin();
  // write car
  auto carGEP = builder.CreateStructGEP(consTy, consPtr, 0, "car.gep");
  builder.CreateStore(&*it, carGEP);
  it++;
  // write cdr
  auto cdrGEP = builder.CreateStructGEP(consTy, consPtr, 1, "cdr.gep");
  builder.CreateStore(&*it, cdrGEP);

  builder.CreateRet(consPtr);

  return F;
}

llvm::Function *emitCar(CodegenContext &codegenContext) {
  auto valuePtrTy = codegenContext.getPtrType();
  auto consTy = codegenContext.getConsTy();
  auto consPtrTy = consTy->getPointerTo();
  auto &builder = codegenContext.builder();

  llvm::FunctionType *FT = llvm::FunctionType::get(valuePtrTy, {consPtrTy},
                                                   /*vararg=*/false);
  llvm::Function *F =
      llvm::Function::Create(FT, llvm::Function::InternalLinkage,
                             "__internal_op_car", codegenContext.module());

  F->addFnAttr(llvm::Attribute::AlwaysInline);
  F->addFnAttr(llvm::Attribute::NoUnwind);
  auto it = F->arg_begin();
  it->setName("cons");

  auto BB = llvm::BasicBlock::Create(codegenContext.context(), "entry", F);
  builder.SetInsertPoint(BB);

  it = F->arg_begin();

  llvm::Value *valPayloadGEP =
      builder.CreateStructGEP(consTy, &*it, 0, "cons.car");
  llvm::Value *valCar =
      builder.CreateLoad(valuePtrTy, valPayloadGEP, "val.unboxed");
  builder.CreateRet(valCar);

  return F;
}

llvm::Function *emitCdr(CodegenContext &codegenContext) {
  auto valuePtrTy = codegenContext.getPtrType();
  auto consTy = codegenContext.getConsTy();
  auto consPtrTy = consTy->getPointerTo();
  auto &builder = codegenContext.builder();

  llvm::FunctionType *FT = llvm::FunctionType::get(valuePtrTy, {consPtrTy},
                                                   /*vararg=*/false);
  llvm::Function *F =
      llvm::Function::Create(FT, llvm::Function::InternalLinkage,
                             "__internal_op_cdr", codegenContext.module());

  F->addFnAttr(llvm::Attribute::AlwaysInline);
  F->addFnAttr(llvm::Attribute::NoUnwind);
  auto it = F->arg_begin();
  it->setName("cons");

  auto BB = llvm::BasicBlock::Create(codegenContext.context(), "entry", F);
  builder.SetInsertPoint(BB);

  it = F->arg_begin();

  llvm::Value *valPayloadGEP =
      builder.CreateStructGEP(consTy, &*it, 1, "cons.cdr");
  llvm::Value *valCdr =
      builder.CreateLoad(valuePtrTy, valPayloadGEP, "val.unboxed");
  builder.CreateRet(valCdr);

  return F;
}

llvm::Function *emitBoxCons(CodegenContext &codegenContext) {
  llvm::StructType *ValueTy = codegenContext.getValueTy();
  auto &builder = codegenContext.builder();
  auto consTyPtr = codegenContext.getConsTy()->getPointerTo();

  llvm::FunctionType *FT = llvm::FunctionType::get(
      codegenContext.getPtrType(), {consTyPtr}, /*vararg=*/false);
  llvm::Function *F = llvm::Function::Create(
      FT, llvm::Function::InternalLinkage, "boxCons", codegenContext.module());

  F->addFnAttr(llvm::Attribute::AlwaysInline);
  F->arg_begin()->setName("x");

  llvm::BasicBlock *BB =
      llvm::BasicBlock::Create(codegenContext.context(), "entry", F);

  builder.SetInsertPoint(BB);

  auto &arg = *F->arg_begin(); // x0

  uint64_t typeSize = codegenContext.module().getDataLayout().getTypeAllocSize(
      codegenContext.getValueTy());

  auto rawBoxed = builder.CreateCall(codegenContext.getArenaAllocator(),
                                     {builder.getInt64(typeSize)}, "boxedVal");

  auto boxed =
      builder.CreateBitCast(rawBoxed, codegenContext.getPtrType(), "cellPtr");

  auto consDescGV = codegenContext.getType("Cons");
  auto typeGEP = builder.CreateStructGEP(ValueTy, boxed, 0, "type.ptr");
  builder.CreateStore(consDescGV, typeGEP);

  auto payloadGEP = builder.CreateStructGEP(ValueTy, boxed, 1, "payload.ptr");
  auto consPtr =
      builder.CreateBitCast(payloadGEP, consTyPtr, "payload.cons.ptr");

  builder.CreateStore(&arg, consPtr);
  builder.CreateRet(boxed);
  return F;
}

llvm::Function *emitUnBoxCons(CodegenContext &codegenContext) {
  llvm::StructType *valueTy = codegenContext.getValueTy();
  auto &builder = codegenContext.builder();
  auto i32Ty = builder.getInt32Ty();
  auto consTyPtr = codegenContext.getConsTy()->getPointerTo();

  llvm::FunctionType *FT = llvm::FunctionType::get(
      consTyPtr, {codegenContext.getPtrType()}, /*vararg=*/false);
  llvm::Function *F =
      llvm::Function::Create(FT, llvm::Function::InternalLinkage, "unboxCons",
                             codegenContext.module());

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

  auto isCons =
      builder.CreateICmpEQ(kind, llvm::ConstantInt::get(i32Ty, /*Cons kind=*/1),
                           "cmp.isConsKind"); // TODO fix magic number

  auto contBB = llvm::BasicBlock::Create(codegenContext.context(), "unbox.ok",
                                         BB->getParent());

  auto panicBB = llvm::BasicBlock::Create(codegenContext.context(),
                                          "unbox.error", BB->getParent());

  builder.CreateCondBr(isCons, contBB, panicBB);

  builder.SetInsertPoint(panicBB);
  llvm::Function *panicFn = codegenContext.getFn("panic");

  builder.CreateCall(panicFn, {});

  builder.CreateUnreachable();

  builder.SetInsertPoint(contBB);

  llvm::Value *valPayloadGEP =
      builder.CreateStructGEP(valueTy, &arg, 1, "val.payload.ptr");
  llvm::Value *valConsPtr =
      builder.CreateBitCast(valPayloadGEP, consTyPtr, "val.cons.ptr");
  llvm::Value *valCons =
      builder.CreateLoad(consTyPtr, valConsPtr, "val.unboxed");

  builder.CreateRet(valCons);

  return F;
}