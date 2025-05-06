#include "prepare.h"

llvm::Value *prepareCMain(CodegenContext &codegenContext,
                          llvm::Function *lispMain) {
  auto &builder = codegenContext.context.builder;
  auto boxedRet = builder.CreateCall(lispMain, {}, "boxedRet");
  llvm::Function *unboxFn = codegenContext.lexenv.getBuiltInFn("unboxInt");
  if (!unboxFn)
    throw std::runtime_error("unboxInt not declared");
  llvm::Value *retI64 = builder.CreateCall(unboxFn, {boxedRet}, "unboxedVal");

  auto ret32 = builder.CreateTrunc(retI64, builder.getInt32Ty(), "ret32");
  return ret32;
}

llvm::Function *emitPanic(CodegenContext &codegenContext) {
  auto voidType = llvm::Type::getVoidTy(codegenContext.context.context);

  llvm::FunctionType *FT =
      llvm::FunctionType::get(voidType, {}, /*vararg=*/false);
  llvm::Function *F =
      llvm::Function::Create(FT, llvm::Function::InternalLinkage, "panic",
                             codegenContext.context.module);

  // entry block + store args into allocas
  llvm::BasicBlock *BB =
      llvm::BasicBlock::Create(codegenContext.context.context, "entry", F);
  llvm::IRBuilder<> builder(BB);

  auto i8Ptr = llvm::PointerType::get(
      llvm::IntegerType::get(codegenContext.context.context, 8), 0);

  auto base = builder.CreateLoad(
      i8Ptr, codegenContext.memory_manager.getArenaPtrGV(), "base");
  auto sizeVal = builder.CreateLoad(
      builder.getInt64Ty(), codegenContext.memory_manager.getArenaSizeGV(),
      "size");
  builder.CreateCall(codegenContext.memory_manager.getmunmapFn(),
                     {base, sizeVal});
  builder.CreateCall(codegenContext.lexenv.getTrapFn(), {});
  builder.CreateUnreachable();
  return F;
}

llvm::Function *emitBoxInt(CodegenContext &codegenContext) {
  llvm::StructType *ValueTy = codegenContext.type_manager.getValueTy();
  auto &builder = codegenContext.context.builder;
  auto i64Ty = builder.getInt64Ty();

  llvm::FunctionType *FT = llvm::FunctionType::get(
      codegenContext.type_manager.getPtrType(), {i64Ty}, /*vararg=*/false);
  llvm::Function *F =
      llvm::Function::Create(FT, llvm::Function::InternalLinkage, "boxInt",
                             codegenContext.context.module);

  F->addFnAttr(llvm::Attribute::AlwaysInline);
  F->arg_begin()->setName("x");

  llvm::BasicBlock *BB =
      llvm::BasicBlock::Create(codegenContext.context.context, "entry", F);

  builder.SetInsertPoint(BB);

  auto &arg = *F->arg_begin(); // x0

  uint64_t typeSize =
      codegenContext.context.module.getDataLayout().getTypeAllocSize(
          codegenContext.type_manager.getValueTy());

  auto rawBoxed =
      builder.CreateCall(codegenContext.memory_manager.getArenaAllocator(),
                         {builder.getInt64(typeSize)}, "boxedVal");

  auto boxed = builder.CreateBitCast(
      rawBoxed, codegenContext.type_manager.getPtrType(), "cellPtr");

  auto intDescGV = codegenContext.type_manager.getType("Int");
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
  llvm::StructType *valueTy = codegenContext.type_manager.getValueTy();
  auto &builder = codegenContext.context.builder;
  auto i64Ty = builder.getInt64Ty();
  auto i32Ty = builder.getInt32Ty();

  llvm::FunctionType *FT = llvm::FunctionType::get(
      i64Ty, {codegenContext.type_manager.getPtrType()}, /*vararg=*/false);
  llvm::Function *F =
      llvm::Function::Create(FT, llvm::Function::InternalLinkage, "unboxInt",
                             codegenContext.context.module);

  F->addFnAttr(llvm::Attribute::AlwaysInline);
  F->arg_begin()->setName("x");

  llvm::BasicBlock *BB =
      llvm::BasicBlock::Create(codegenContext.context.context, "entry", F);

  builder.SetInsertPoint(BB);

  auto &arg = *F->arg_begin(); // x0

  auto tdGEP = builder.CreateStructGEP(valueTy, &arg, 0, "type.ptr");
  auto tdPtr = builder.CreateLoad(
      codegenContext.type_manager.getTypeDescTy()->getPointerTo(), tdGEP,
      "type.description");

  auto kindPtr = builder.CreateStructGEP(
      codegenContext.type_manager.getTypeDescTy(), tdPtr, 1, "kind.ptr");
  auto kind = builder.CreateLoad(i32Ty, kindPtr, "kind");

  auto isInt = builder.CreateICmpEQ(
      kind, llvm::ConstantInt::get(i32Ty, /*Int kind=*/0), "cmp.isIntKind");

  auto contBB = llvm::BasicBlock::Create(codegenContext.context.context,
                                         "unbox.ok", BB->getParent());

  auto panicBB = llvm::BasicBlock::Create(codegenContext.context.context,
                                          "unbox.error", BB->getParent());

  builder.CreateCondBr(isInt, contBB, panicBB);

  builder.SetInsertPoint(panicBB);
  llvm::Function *panicFn = codegenContext.lexenv.getBuiltInFn("panic");

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
  auto valuePtrTy = codegenContext.type_manager.getPtrType();
  auto consTy = codegenContext.type_manager.getConsTy();
  auto consPtrTy = consTy->getPointerTo();
  auto &builder = codegenContext.context.builder;

  llvm::FunctionType *FT =
      llvm::FunctionType::get(consPtrTy, {valuePtrTy, valuePtrTy},
                              /*vararg=*/false);
  llvm::Function *F = llvm::Function::Create(
      FT, llvm::Function::InternalLinkage, getBuiltInName("cons"),
      codegenContext.context.module);

  F->addFnAttr(llvm::Attribute::AlwaysInline);
  auto it = F->arg_begin();
  it->setName("car");
  ++it;
  it->setName("cdr");

  auto BB =
      llvm::BasicBlock::Create(codegenContext.context.context, "entry", F);
  builder.SetInsertPoint(BB);

  uint64_t consTypeSize =
      codegenContext.context.module.getDataLayout().getTypeAllocSize(
          codegenContext.type_manager.getConsTy());
  auto rawCell =
      builder.CreateCall(codegenContext.memory_manager.getArenaAllocator(),
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
  auto valuePtrTy = codegenContext.type_manager.getPtrType();
  auto consTy = codegenContext.type_manager.getConsTy();
  auto consPtrTy = consTy->getPointerTo();
  auto &builder = codegenContext.context.builder;

  llvm::FunctionType *FT = llvm::FunctionType::get(valuePtrTy, {consPtrTy},
                                                   /*vararg=*/false);
  llvm::Function *F = llvm::Function::Create(
      FT, llvm::Function::InternalLinkage, getBuiltInName("car"),
      codegenContext.context.module);

  F->addFnAttr(llvm::Attribute::AlwaysInline);
  F->addFnAttr(llvm::Attribute::NoUnwind);
  auto it = F->arg_begin();
  it->setName("cons");

  auto BB =
      llvm::BasicBlock::Create(codegenContext.context.context, "entry", F);
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
  auto valuePtrTy = codegenContext.type_manager.getPtrType();
  auto consTy = codegenContext.type_manager.getConsTy();
  auto consPtrTy = consTy->getPointerTo();
  auto &builder = codegenContext.context.builder;

  llvm::FunctionType *FT = llvm::FunctionType::get(valuePtrTy, {consPtrTy},
                                                   /*vararg=*/false);
  llvm::Function *F = llvm::Function::Create(
      FT, llvm::Function::InternalLinkage, getBuiltInName("cdr"),
      codegenContext.context.module);

  F->addFnAttr(llvm::Attribute::AlwaysInline);
  F->addFnAttr(llvm::Attribute::NoUnwind);
  auto it = F->arg_begin();
  it->setName("cons");

  auto BB =
      llvm::BasicBlock::Create(codegenContext.context.context, "entry", F);
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
  llvm::StructType *ValueTy = codegenContext.type_manager.getValueTy();
  auto &builder = codegenContext.context.builder;
  auto consTyPtr = codegenContext.type_manager.getConsTy()->getPointerTo();

  llvm::FunctionType *FT = llvm::FunctionType::get(
      codegenContext.type_manager.getPtrType(), {consTyPtr}, /*vararg=*/false);
  llvm::Function *F =
      llvm::Function::Create(FT, llvm::Function::InternalLinkage, "boxCons",
                             codegenContext.context.module);

  F->addFnAttr(llvm::Attribute::AlwaysInline);
  F->arg_begin()->setName("x");

  llvm::BasicBlock *BB =
      llvm::BasicBlock::Create(codegenContext.context.context, "entry", F);

  builder.SetInsertPoint(BB);

  auto &arg = *F->arg_begin(); // x0

  uint64_t typeSize =
      codegenContext.context.module.getDataLayout().getTypeAllocSize(
          codegenContext.type_manager.getValueTy());

  auto rawBoxed =
      builder.CreateCall(codegenContext.memory_manager.getArenaAllocator(),
                         {builder.getInt64(typeSize)}, "boxedVal");

  auto boxed = builder.CreateBitCast(
      rawBoxed, codegenContext.type_manager.getPtrType(), "cellPtr");

  auto consDescGV = codegenContext.type_manager.getType("Cons");
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
  llvm::StructType *valueTy = codegenContext.type_manager.getValueTy();
  auto &builder = codegenContext.context.builder;
  auto i32Ty = builder.getInt32Ty();
  auto consTyPtr = codegenContext.type_manager.getConsTy()->getPointerTo();

  llvm::FunctionType *FT = llvm::FunctionType::get(
      consTyPtr, {codegenContext.type_manager.getPtrType()}, /*vararg=*/false);
  llvm::Function *F =
      llvm::Function::Create(FT, llvm::Function::InternalLinkage, "unboxCons",
                             codegenContext.context.module);

  F->addFnAttr(llvm::Attribute::AlwaysInline);
  F->arg_begin()->setName("x");

  llvm::BasicBlock *BB =
      llvm::BasicBlock::Create(codegenContext.context.context, "entry", F);

  builder.SetInsertPoint(BB);

  auto &arg = *F->arg_begin(); // x0

  auto tdGEP = builder.CreateStructGEP(valueTy, &arg, 0, "type.ptr");
  auto tdPtr = builder.CreateLoad(
      codegenContext.type_manager.getTypeDescTy()->getPointerTo(), tdGEP,
      "type.description");

  auto kindPtr = builder.CreateStructGEP(
      codegenContext.type_manager.getTypeDescTy(), tdPtr, 1, "kind.ptr");
  auto kind = builder.CreateLoad(i32Ty, kindPtr, "kind");

  auto isCons =
      builder.CreateICmpEQ(kind, llvm::ConstantInt::get(i32Ty, /*Cons kind=*/1),
                           "cmp.isConsKind"); // TODO fix magic number

  auto contBB = llvm::BasicBlock::Create(codegenContext.context.context,
                                         "unbox.ok", BB->getParent());

  auto panicBB = llvm::BasicBlock::Create(codegenContext.context.context,
                                          "unbox.error", BB->getParent());

  builder.CreateCondBr(isCons, contBB, panicBB);

  builder.SetInsertPoint(panicBB);
  llvm::Function *panicFn = codegenContext.lexenv.getBuiltInFn("panic");

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

llvm::Function *emitUnBoxFn(CodegenContext &codegenContext) {
  llvm::StructType *valueTy = codegenContext.type_manager.getValueTy();
  auto &builder = codegenContext.context.builder;
  auto i32Ty = builder.getInt32Ty();

  llvm::FunctionType *FT = llvm::FunctionType::get(
      codegenContext.type_manager.getPtrType(),
      {codegenContext.type_manager.getPtrType()}, /*vararg=*/false);
  llvm::Function *F =
      llvm::Function::Create(FT, llvm::Function::InternalLinkage, "unboxFn",
                             codegenContext.context.module);

  F->addFnAttr(llvm::Attribute::AlwaysInline);
  F->arg_begin()->setName("x");

  llvm::BasicBlock *BB =
      llvm::BasicBlock::Create(codegenContext.context.context, "entry", F);

  builder.SetInsertPoint(BB);

  auto &arg = *F->arg_begin(); // x0

  auto tdGEP = builder.CreateStructGEP(valueTy, &arg, 0, "type.ptr");
  auto tdPtr = builder.CreateLoad(
      codegenContext.type_manager.getTypeDescTy()->getPointerTo(), tdGEP,
      "type.description");

  auto kindPtr = builder.CreateStructGEP(
      codegenContext.type_manager.getTypeDescTy(), tdPtr, 1, "kind.ptr");
  auto kind = builder.CreateLoad(i32Ty, kindPtr, "kind");

  auto isFn = builder.CreateICmpEQ(
      kind, llvm::ConstantInt::get(i32Ty, /*Int kind=*/-1), "cmp.isFnKind");

  auto contBB = llvm::BasicBlock::Create(codegenContext.context.context,
                                         "unbox.ok", BB->getParent());

  auto panicBB = llvm::BasicBlock::Create(codegenContext.context.context,
                                          "unbox.error", BB->getParent());

  builder.CreateCondBr(isFn, contBB, panicBB);

  builder.SetInsertPoint(panicBB);
  llvm::Function *panicFn = codegenContext.lexenv.getBuiltInFn("panic");

  builder.CreateCall(panicFn, {});

  builder.CreateUnreachable();

  builder.SetInsertPoint(contBB);

  llvm::Value *valPayloadGEP =
      builder.CreateStructGEP(valueTy, &arg, 1, "val.payload.ptr");
  llvm::Value *valI64 = builder.CreateLoad(
      codegenContext.type_manager.getPtrType(), valPayloadGEP, "val.unboxed");
  builder.CreateRet(valI64);

  return F;
}