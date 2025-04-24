#include "meta.h"

CodegenContext::CodegenContext()
    : context_(std::make_unique<llvm::LLVMContext>()),
      builder_(std::make_unique<llvm::IRBuilder<>>(*context_)),
      module_(std::make_unique<llvm::Module>("my lisp", *context_)),
      typeDescTy_(makeTypeDescType(*this)),
      valueTy_(makeValueType(*this, typeDescTy_)),
      ptrTy_(llvm::PointerType::getUnqual(valueTy_)) {

  auto *i8Ptr = llvm::PointerType::get(llvm::IntegerType::get(context(), 8), 0);
  auto *i64Ty = llvm::Type::getInt64Ty(context());
  auto *i32Ty = llvm::Type::getInt32Ty(context());

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
  auto *mmapFT = llvm::FunctionType::get(i8Ptr, mmapArgs, /*vararg*/ false);
  mmapFn_ = cast<llvm::Function>(
      module().getOrInsertFunction("mmap", mmapFT).getCallee());

  // int munmap(void*, size_t);
  auto *munmapFT =
      llvm::FunctionType::get(i32Ty, {i8Ptr, i64Ty}, /*vararg*/ false);
  munmapFn_ = cast<llvm::Function>(
      module().getOrInsertFunction("munmap", munmapFT).getCallee());

  arenaPtrGV_ = new llvm::GlobalVariable(
      module(), i8Ptr,
      /*isConstant=*/false, llvm::GlobalValue::InternalLinkage,
      llvm::Constant::getNullValue(i8Ptr), "arenaBase");

  arenaNextGV_ = new llvm::GlobalVariable(
      module(), i64Ty,
      /*isConstant=*/false, llvm::GlobalValue::InternalLinkage,
      llvm::ConstantInt::get(i64Ty, 0), "arenaNext");

  uint64_t arenaCapacity = 1024 * 1024 * 8;
  arenaSizeGV_ = new llvm::GlobalVariable(
      module(), i64Ty,
      /*isConstant=*/false, llvm::GlobalValue::InternalLinkage,
      llvm::ConstantInt::get(i64Ty, arenaCapacity), "arenaSize");

  // allocValue(): Value* ()
  llvm::FunctionType *FT = llvm::FunctionType::get(
      /*RetTy=*/getPtrType(),
      /*Params=*/{},
      /*isVarArg=*/false);
  arenaAllocValueFn_ = llvm::Function::Create(
      FT, llvm::Function::InternalLinkage, "arenaAllocValue", module());

  defineArenaAlloc();
}

// Getter methods
llvm::LLVMContext &CodegenContext::context() { return *context_; }

llvm::IRBuilder<> &CodegenContext::builder() { return *builder_; }

llvm::Module &CodegenContext::module() { return *module_; }

std::map<std::string, llvm::AllocaInst *> &CodegenContext::named_values() {
  return named_values_;
}

std::vector<std::unordered_map<std::string, llvm::BasicBlock *>> &
CodegenContext::tagEnvs() {
  return tagEnvs_;
}

llvm::StructType *CodegenContext::getTypeDescTy() { return typeDescTy_; }

llvm::StructType *CodegenContext::getValueTy() { return valueTy_; }

llvm::PointerType *CodegenContext::getPtrType() { return ptrTy_; }

llvm::Function *CodegenContext::getmmapFn() { return mmapFn_; }

llvm::Function *CodegenContext::getmunmapFn() { return munmapFn_; }

llvm::Function *CodegenContext::getArenaAllocator() {
  return arenaAllocValueFn_;
}

llvm::GlobalVariable *CodegenContext::getArenaPtrGV() { return arenaPtrGV_; }

llvm::GlobalVariable *CodegenContext::getArenaNextGV() { return arenaNextGV_; }

llvm::GlobalVariable *CodegenContext::getArenaSizeGV() { return arenaSizeGV_; }

std::unordered_map<std::string, llvm::BasicBlock *> &
CodegenContext::lastTagEnv() {
  if (tagEnvs_.empty())
    throw std::runtime_error("Go outside of tagbody");
  return tagEnvs_.back();
}

void CodegenContext::defineArenaAlloc() {
  auto &C = context();
  auto &B = builder();
  auto DL = module().getDataLayout();

  // Create its entry block
  llvm::BasicBlock *entry =
      llvm::BasicBlock::Create(C, "entry", arenaAllocValueFn_);
  B.SetInsertPoint(entry);

  // Load the current index
  llvm::Value *idx =
      B.CreateLoad(getArenaNextGV()->getValueType(), getArenaNextGV(), "idx");

  // Compute the byte-offset = idx * sizeof(ValueTy)
  uint64_t elemSize = DL.getTypeAllocSize(getValueTy());
  llvm::Value *offset = B.CreateMul(
      idx, llvm::ConstantInt::get(llvm::Type::getInt64Ty(C), elemSize),
      "offsetBytes");

  // Load the base pointer (i8*)
  llvm::Value *base =
      B.CreateLoad(getArenaPtrGV()->getValueType(), getArenaPtrGV(), "base");

  // Compute raw cell ptr = base + offset
  //    (getelementptr i8, i8* base, i64 offset)
  llvm::Value *rawCellPtr =
      B.CreateInBoundsGEP(llvm::Type::getInt8Ty(C), base, offset, "cellRawPtr");

  // Bump the index: idx+1
  llvm::Value *nextIdx = B.CreateAdd(
      idx, llvm::ConstantInt::get(llvm::Type::getInt64Ty(C), 1), "idxPlus");
  B.CreateStore(nextIdx, getArenaNextGV());

  // Cast raw i8* → Value*
  llvm::Value *cellPtr = B.CreateBitCast(rawCellPtr, getPtrType(), "cellPtr");

  // Return the new cell pointer
  B.CreateRet(cellPtr);

  // (Optionally, you could add a bounds‐check before step 4 and trap if
  //  idx >= capacity.)
}