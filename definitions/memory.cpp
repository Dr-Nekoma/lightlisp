#include "meta.h"

CodegenContext::Memorymanager::Memorymanager(CodegenContext &codegenContext) {
  auto &[context, builder, module] = codegenContext.context;
  auto i8Ptr = llvm::PointerType::get(context, 0);
  auto i64Ty = llvm::Type::getInt64Ty(context);
  auto i32Ty = llvm::Type::getInt32Ty(context);

  // void* mmap(void*, size_t, int, int, int, off_t);
  // on Linux off_t is 64-bit:
  std::vector<llvm::Type *> mmapArgs = {
      i8Ptr, // addr
      i64Ty, // length
      i32Ty, // prot
      i32Ty, // flags
      i32Ty, // fd
      i64Ty  // offset
  };
  auto mmapFT = llvm::FunctionType::get(i8Ptr, mmapArgs, /*vararg*/ false);
  mmapFn_ = cast<llvm::Function>(
      module.getOrInsertFunction("mmap", mmapFT).getCallee());

  // int munmap(void*, size_t);
  auto munmapFT =
      llvm::FunctionType::get(i32Ty, {i8Ptr, i64Ty}, /*vararg*/ false);
  munmapFn_ = cast<llvm::Function>(
      module.getOrInsertFunction("munmap", munmapFT).getCallee());

  arenaPtrGV_ = new llvm::GlobalVariable(
      module, i8Ptr,
      /*isConstant=*/false, llvm::GlobalValue::InternalLinkage,
      llvm::Constant::getNullValue(i8Ptr), "arenaBase");

  arenaNextGV_ = new llvm::GlobalVariable(
      module, i64Ty,
      /*isConstant=*/false, llvm::GlobalValue::InternalLinkage,
      llvm::ConstantInt::get(i64Ty, 0), "arenaNext");

  uint64_t arenaCapacity = 1024 * 1024 * 8;
  arenaSizeGV_ = new llvm::GlobalVariable(
      module, i64Ty,
      /*isConstant=*/false, llvm::GlobalValue::InternalLinkage,
      llvm::ConstantInt::get(i64Ty, arenaCapacity), "arenaSize");

  prepareArena(codegenContext);

  arenaAllocValueFn_ = defineArenaAlloc(codegenContext.context);
}

llvm::Function *
CodegenContext::Memorymanager::defineArenaAlloc(IRGenContext &irgc) {
  auto &C = irgc.context;
  auto &B = irgc.builder;
  auto DL = irgc.module.getDataLayout();

  auto i64Ty = llvm::Type::getInt64Ty(C);
  auto i8Ptr = llvm::PointerType::get(C, 0);

  auto FT = llvm::FunctionType::get(
      /*RetTy=*/i8Ptr,
      /*Params=*/{i64Ty},
      /*isVarArg=*/false);
  auto allocator = llvm::Function::Create(FT, llvm::Function::InternalLinkage,
                                          "arenaAllocValue", irgc.module);

  auto it = allocator->arg_begin();
  it->setName("size");

  auto entry = llvm::BasicBlock::Create(C, "entry", allocator);
  B.SetInsertPoint(entry);

  // Load the current index
  auto idx =
      B.CreateLoad(getArenaNextGV()->getValueType(), getArenaNextGV(), "idx");
  // Compute the byte-offset = idx + size
  auto size = &*it;
  llvm::Value *offset = B.CreateAdd(idx, size, "offsetBytes");

  // Load the base pointer (i8*)
  auto base =
      B.CreateLoad(getArenaPtrGV()->getValueType(), getArenaPtrGV(), "base");

  // Compute raw cell ptr = base + idx
  //    (getelementptr i8, i8* base, i64 offset)
  llvm::Value *rawCellPtr =
      B.CreateInBoundsGEP(llvm::Type::getInt8Ty(C), base, idx, "cellRawPtr");

  B.CreateStore(offset, getArenaNextGV());

  // No cast, this is raw allocated space
  B.CreateRet(rawCellPtr);

  return allocator;
  // (Optionally, could add a bounds‐check before step 4 and trap if
}

llvm::Function *CodegenContext::Memorymanager::getmmapFn() { return mmapFn_; }

llvm::Function *CodegenContext::Memorymanager::getmunmapFn() {
  return munmapFn_;
}

llvm::Function *CodegenContext::Memorymanager::getArenaAllocator() {
  return arenaAllocValueFn_;
}

llvm::GlobalVariable *CodegenContext::Memorymanager::getArenaPtrGV() {
  return arenaPtrGV_;
}

llvm::GlobalVariable *CodegenContext::Memorymanager::getArenaNextGV() {
  return arenaNextGV_;
}

llvm::GlobalVariable *CodegenContext::Memorymanager::getArenaSizeGV() {
  return arenaSizeGV_;
}

void CodegenContext::Memorymanager::prepareArena(
    CodegenContext &codegenContext) {
  auto &[context, builder, module] = codegenContext.context;
  auto i8Ptr = llvm::PointerType::get(context, 0);

  int PROT_READ = 1;
  int PROT_WRITE = 2;
  int MAP_PRIVATE = 2;
  int MAP_ANON = 0x20;
  // constants for prot/flags
  auto prot = builder.getInt32(PROT_READ | PROT_WRITE);
  auto flags = builder.getInt32(MAP_ANON | MAP_PRIVATE);
  auto fd = builder.getInt32(-1);
  auto off = builder.getInt64(0);

  llvm::FunctionType *FT =
      llvm::FunctionType::get(llvm::Type::getVoidTy(context), {i8Ptr},
                              /*vararg=*/false);
  llvm::Function *F = llvm::Function::Create(
      FT, llvm::Function::InternalLinkage, "prepare.arena", module);

  llvm::BasicBlock *BB = llvm::BasicBlock::Create(context, "entry", F);

  builder.SetInsertPoint(BB);
  auto sizeVal =
      builder.CreateLoad(builder.getInt64Ty(), getArenaSizeGV(), "size");
  // call mmap(NULL, size, prot, flags, fd, off)
  auto basePtr = builder.CreateCall(
      getmmapFn(),
      {llvm::Constant::getNullValue(i8Ptr), sizeVal, prot, flags, fd, off},
      "arenaBaseRaw");
  // store into your global
  builder.CreateStore(basePtr, getArenaPtrGV());
  builder.CreateRet(nullptr);

  codegenContext.addCtor(0, F);
}

void CodegenContext::Memorymanager::munmapArena(IRGenContext &irgc) {
  auto &builder = irgc.builder;
  auto i8Ptr = llvm::PointerType::get(irgc.context, 0);

  auto base = builder.CreateLoad(i8Ptr, getArenaPtrGV(), "base");
  auto sizeVal2 =
      builder.CreateLoad(builder.getInt64Ty(), getArenaSizeGV(), "size2");
  builder.CreateCall(getmunmapFn(), {base, sizeVal2});
}