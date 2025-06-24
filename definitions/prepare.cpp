#include "prepare.h"

llvm::Value *prepareCMain(CodegenContext &codegenContext) {
  auto &[context, builder, module] = codegenContext.context;
  Call mainCall(std::make_unique<Symbol>("lisp_main"), {});
  auto boxedRet = mainCall.codegen(codegenContext).get();
  auto int64Val =
      codegenContext.type_manager.checkAndUnpack(boxedRet, Type::Int);

  auto ret32 = builder.CreateTrunc(int64Val, builder.getInt32Ty(), "ret32");
  return ret32;
}

llvm::Function *emitPanic(CodegenContext &codegenContext) {
  auto &[context, builder, module] = codegenContext.context;
  auto voidType = llvm::Type::getVoidTy(context);

  llvm::FunctionType *FT = llvm::FunctionType::get(
      voidType, {builder.getInt32Ty(), builder.getInt32Ty()}, /*vararg=*/false);
  llvm::Function *F = llvm::Function::Create(
      FT, llvm::Function::InternalLinkage, "panic", module);

  // entry block + store args into allocas
  llvm::BasicBlock *BB = llvm::BasicBlock::Create(context, "entry", F);
  builder.SetInsertPoint(BB);
  auto ptrType = codegenContext.type_manager.ptrType;

  auto base = builder.CreateLoad(
      ptrType, codegenContext.memory_manager.getArenaPtrGV(), "base");
  auto sizeVal = builder.CreateLoad(
      builder.getInt64Ty(), codegenContext.memory_manager.getArenaSizeGV(),
      "size");
  builder.CreateCall(codegenContext.memory_manager.getMunmapFn(),
                     {base, sizeVal});
  auto it = F->arg_begin();
  auto errcode = &*it;
  it++;
  auto proper_code = &*it;
  builder.CreateCall(codegenContext.lexenv.getTrapFn(), {errcode, proper_code});
  builder.CreateUnreachable();
  return F;
}

llvm::Function *emitCons(CodegenContext &codegenContext) {
  auto ptrType = codegenContext.type_manager.ptrType;
  auto consType = codegenContext.type_manager.consType;
  auto &[context, builder, module] = codegenContext.context;

  llvm::FunctionType *FT = llvm::FunctionType::get(ptrType, {ptrType, ptrType},
                                                   /*vararg=*/false);
  llvm::Function *F = llvm::Function::Create(
      FT, llvm::Function::InternalLinkage, getBuiltInName("cons"), module);

  F->addFnAttr(llvm::Attribute::AlwaysInline);
  auto it = F->arg_begin();
  it->setName("car");
  ++it;
  it->setName("cdr");

  auto BB = llvm::BasicBlock::Create(context, "entry", F);
  builder.SetInsertPoint(BB);

  uint64_t consTypeSize = module.getDataLayout().getTypeAllocSize(consType);
  auto rawCell =
      builder.CreateCall(codegenContext.memory_manager.getArenaAllocator(),
                         {builder.getInt64(consTypeSize)}, "rawCons");

  auto consPtr = builder.CreateBitCast(rawCell, ptrType, "cons.ptr");

  it = F->arg_begin();
  // write car
  auto carGEP = builder.CreateStructGEP(consType, consPtr, 0, "car.gep");
  builder.CreateStore(&*it, carGEP);
  it++;
  // write cdr
  auto cdrGEP = builder.CreateStructGEP(consType, consPtr, 1, "cdr.gep");
  builder.CreateStore(&*it, cdrGEP);

  builder.CreateRet(consPtr);

  return F;
}

llvm::Function *emitCar(CodegenContext &codegenContext) {
  auto ptrType = codegenContext.type_manager.ptrType;
  auto consType = codegenContext.type_manager.consType;
  auto &[context, builder, module] = codegenContext.context;

  llvm::FunctionType *FT = llvm::FunctionType::get(ptrType, {ptrType},
                                                   /*vararg=*/false);
  llvm::Function *F = llvm::Function::Create(
      FT, llvm::Function::InternalLinkage, getBuiltInName("car"), module);

  F->addFnAttr(llvm::Attribute::AlwaysInline);
  F->addFnAttr(llvm::Attribute::NoUnwind);
  auto it = F->arg_begin();
  it->setName("cons");

  auto BB = llvm::BasicBlock::Create(context, "entry", F);
  builder.SetInsertPoint(BB);

  it = F->arg_begin();

  llvm::Value *valPayloadGEP =
      builder.CreateStructGEP(consType, &*it, 0, "cons.car");
  llvm::Value *valCar = builder.CreateLoad(ptrType, valPayloadGEP, "car.val");
  builder.CreateRet(valCar);

  return F;
}

llvm::Function *emitCdr(CodegenContext &codegenContext) {
  auto ptrType = codegenContext.type_manager.ptrType;
  auto consType = codegenContext.type_manager.consType;
  auto &[context, builder, module] = codegenContext.context;

  llvm::FunctionType *FT = llvm::FunctionType::get(ptrType, {ptrType},
                                                   /*vararg=*/false);
  llvm::Function *F = llvm::Function::Create(
      FT, llvm::Function::InternalLinkage, getBuiltInName("cdr"), module);

  F->addFnAttr(llvm::Attribute::AlwaysInline);
  F->addFnAttr(llvm::Attribute::NoUnwind);
  auto it = F->arg_begin();
  it->setName("cons");

  auto BB = llvm::BasicBlock::Create(context, "entry", F);
  builder.SetInsertPoint(BB);

  it = F->arg_begin();

  llvm::Value *valPayloadGEP =
      builder.CreateStructGEP(consType, &*it, 1, "cons.cdr");
  llvm::Value *valCdr = builder.CreateLoad(ptrType, valPayloadGEP, "cdr.val");
  builder.CreateRet(valCdr);

  return F;
}

llvm::Function *emitSetCar(CodegenContext &codegenContext) {
  auto ptrType = codegenContext.type_manager.ptrType;
  auto consType = codegenContext.type_manager.consType;
  auto &[context, builder, module] = codegenContext.context;

  llvm::FunctionType *FT = llvm::FunctionType::get(ptrType, {ptrType, ptrType},
                                                   /*vararg=*/false);
  llvm::Function *F = llvm::Function::Create(
      FT, llvm::Function::InternalLinkage, getBuiltInName("setcar"), module);

  auto it = F->arg_begin();
  it->setName("cons");
  it++;
  it->setName("newval");

  auto BB = llvm::BasicBlock::Create(context, "entry", F);
  builder.SetInsertPoint(BB);

  it = F->arg_begin();

  llvm::Value *valPayloadGEP =
      builder.CreateStructGEP(consType, &*it, 0, "cons.car");
  it++;
  builder.CreateStore(&*it, valPayloadGEP);
  builder.CreateRet(&*it);
  return F;
}

llvm::Function *emitSetCdr(CodegenContext &codegenContext) {
  auto ptrType = codegenContext.type_manager.ptrType;
  auto consType = codegenContext.type_manager.consType;
  auto &[context, builder, module] = codegenContext.context;

  llvm::FunctionType *FT = llvm::FunctionType::get(ptrType, {ptrType, ptrType},
                                                   /*vararg=*/false);
  llvm::Function *F = llvm::Function::Create(
      FT, llvm::Function::InternalLinkage, getBuiltInName("setcdr"), module);

  auto it = F->arg_begin();
  it->setName("cons");
  it++;
  it->setName("newval");

  auto BB = llvm::BasicBlock::Create(context, "entry", F);
  builder.SetInsertPoint(BB);

  it = F->arg_begin();

  llvm::Value *valPayloadGEP =
      builder.CreateStructGEP(consType, &*it, 1, "cons.cdr");
  it++;
  builder.CreateStore(&*it, valPayloadGEP);
  builder.CreateRet(&*it);
  return F;
}