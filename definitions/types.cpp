#include "types.h"

llvm::StructType *makeTypeDescType(CodegenContext &codegenContext) {
  auto *i8Ptr = llvm::PointerType::get(
      llvm::IntegerType::get(codegenContext.context(), 8), 0);
  auto *i32Ty = llvm::Type::getInt32Ty(codegenContext.context());
  auto *TD = llvm::StructType::create(codegenContext.context(), "TypeDesc");
  TD->setBody({i8Ptr, i32Ty}, /*isPacked=*/false);
  return TD;
}

llvm::StructType *makeValueType(CodegenContext &codegenContext,
                                llvm::StructType *TypeDescTy) {
  auto *TDPtr = llvm::PointerType::get(TypeDescTy, 0);
  auto *payload =
      llvm::ArrayType::get(llvm::Type::getInt8Ty(codegenContext.context()), 8);
  auto *ValTy = llvm::StructType::create(codegenContext.context(), "Value");
  ValTy->setBody({TDPtr, payload}, /*isPacked=*/false);
  return ValTy;
}

void createBuiltinTypeDescs(CodegenContext &codegenContext) {
  auto TypeDescTy = codegenContext.getTypeDescTy();
  auto *i32Ty = llvm::Type::getInt32Ty(codegenContext.context());

  auto mkDesc = [&](llvm::StringRef name, int kind) -> llvm::GlobalVariable * {
    // name string constant
    auto *nameConst =
        llvm::ConstantDataArray::getString(codegenContext.context(), name);
    auto *nameGV = new llvm::GlobalVariable(
        codegenContext.module(), nameConst->getType(), true,
        llvm::GlobalValue::PrivateLinkage, nameConst, name + ".str");
    // ptr to name
    auto *zero = llvm::ConstantInt::get(i32Ty, 0);
    llvm::SmallVector<llvm::Constant *, 2> idxs = {zero, zero};
    auto *namePtr = llvm::ConstantExpr::getInBoundsGetElementPtr(
        nameConst->getType(), nameGV, idxs);
    // struct initializer { i8* namePtr, i32 kind }
    auto *init = llvm::ConstantStruct::get(
        TypeDescTy, {namePtr, llvm::ConstantInt::get(i32Ty, kind)});
    // the global TypeDesc
    return new llvm::GlobalVariable(codegenContext.module(), TypeDescTy, true,
                                    llvm::GlobalValue::ExternalLinkage, init,
                                    "type." + name);
  };

  // e.g. kind 0 = Int, 1 = Float
  mkDesc("Int", 0);
  // mkDesc("Cell", 1);
  //  users can add: mkDesc("MyStruct", 2), etc.
}
/*
void example(Module &M) {
  auto &C = M.getContext();

  // build types
  StructType *TypeDescTy = makeTypeDescType(C);
  StructType *ValueTy = makeValueType(C, TypeDescTy);
  PointerType *ValuePtr = PointerType::get(ValueTy, 0);

  // emit the built-in descriptors
  createBuiltinTypeDescs(M, TypeDescTy);

  IRBuilder<> B(C);
  // … in your function …
  FunctionType *FT = FunctionType::get(B.getInt32Ty(), {}, false);
  Function *F = Function::Create(FT, GlobalValue::ExternalLinkage, "main", &M);
  BasicBlock *BB = BasicBlock::Create(C, "entry", F);
  B.SetInsertPoint(BB);

  // 3) Allocate a Value on the stack
  Value *valAlloca = B.CreateAlloca(ValueTy, nullptr, "val");

  // look up the Int TypeDesc global
  GlobalVariable *intDescGV = M.getNamedGlobal("type.Int");
  Value *intDescPtr = B.CreateLoad(
      TypeDescTy->getPointerTo(),
      B.CreateBitCast(intDescGV, TypeDescTy->getPointerTo()), "intDesc");

  // store the type pointer into field 0
  Value *typeFieldPtr = B.CreateStructGEP(ValueTy, valAlloca, 0, "type.ptr");
  B.CreateStore(intDescPtr, typeFieldPtr);

  // store payload (i64) into field 1
  Value *payloadPtr = B.CreateStructGEP(ValueTy, valAlloca, 1, "payload.ptr");
  Value *i64Ptr =
      B.CreateBitCast(payloadPtr, PointerType::get(B.getInt64Ty(), 0));
  B.CreateStore(B.getInt64(42), i64Ptr);

  // 4) Later: load type pointer, then inspect its .kind or compare ptr ==
  // intDescGV
  Value *loadedTypePtr =
      B.CreateLoad(TypeDescTy->getPointerTo(), typeFieldPtr, "ldt");
  Value *cmpIsInt = B.CreateICmpEQ(loadedTypePtr, intDescGV, "isInt");
  // … then branch or switch on cmpIsInt …

  B.CreateRet(B.getInt32(0));
}
*/