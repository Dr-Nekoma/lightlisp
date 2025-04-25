#include "util.h"

llvm::AllocaInst *CreateEntryBlockAlloca(CodegenContext &codegenContext,
                                         llvm::Function *currentFn,
                                         llvm::Type *type,
                                         const std::string &VarName) {
  llvm::IRBuilder<> TmpB(&currentFn->getEntryBlock(),
                         currentFn->getEntryBlock().begin());
  return TmpB.CreateAlloca(type, nullptr, VarName);
}

llvm::CallInst *boxIntVal(CodegenContext &codegenContext, llvm::Value *val,
                          std::string &&name) {
  llvm::StructType *ValueTy = codegenContext.getValueTy();
  auto &builder = codegenContext.builder();

  auto boxed = builder.CreateCall(codegenContext.getArenaAllocator(), {}, name);

  auto intDescGV = codegenContext.module().getNamedGlobal("type.Int");
  auto typeGEP = builder.CreateStructGEP(ValueTy, boxed, 0, "type.ptr");
  builder.CreateStore(intDescGV, typeGEP);

  //  store the raw i64 payload into the 8-byte union
  auto payloadGEP = builder.CreateStructGEP(ValueTy, boxed, 1, "payload.ptr");
  auto i64Ptr = builder.CreateBitCast(
      payloadGEP, llvm::PointerType::get(builder.getInt64Ty(), 0),
      "payload.i64.ptr");
  builder.CreateStore(val, i64Ptr);

  return boxed;
}

llvm::Value *unboxIntVal(CodegenContext &codegenContext, llvm::Value *val) {
  auto &builder = codegenContext.builder();
  auto valueTy = codegenContext.getValueTy();

  llvm::Value *valPayloadGEP =
      builder.CreateStructGEP(valueTy, val, 1, "val.payload.ptr");
  // reinterpret [8 x i8]* as i64*
  llvm::Value *valI64Ptr = builder.CreateBitCast(
      valPayloadGEP, builder.getInt64Ty()->getPointerTo(), "val.i64.ptr");
  // load the raw i64
  llvm::Value *valI64 =
      builder.CreateLoad(builder.getInt64Ty(), valI64Ptr, "val.unboxed");
  return valI64;
}

std::string getBuiltInName(std::string &&name) {
  return "__internal_op_" + std::string(name);
}