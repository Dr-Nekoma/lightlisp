#include "prepare.h"

llvm::Value *prepareCMain(CodegenContext &codegenContext) {
  auto &builder = codegenContext.context.builder;
  Call mainCall("lisp_main", {});
  auto boxedRet = mainCall.codegen(codegenContext);
  auto int64Val = codegenContext.type_manager.checkAndUnpack(
      codegenContext, boxedRet, Type::Int);

  auto ret32 = builder.CreateTrunc(int64Val, builder.getInt32Ty(), "ret32");
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
  sizeVal->setAlignment(llvm::Align(8));
  builder.CreateCall(codegenContext.memory_manager.getmunmapFn(),
                     {base, sizeVal});
  builder.CreateCall(codegenContext.lexenv.getTrapFn(), {});
  builder.CreateUnreachable();
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
      builder.CreateLoad(valuePtrTy, valPayloadGEP, "car.val");
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
      builder.CreateLoad(valuePtrTy, valPayloadGEP, "cdr.val");
  builder.CreateRet(valCdr);

  return F;
}
