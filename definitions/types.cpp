#include "types.h"

llvm::StructType *makeTypeDescType(CodegenContext &codegenContext) {
  auto i8Ptr = llvm::PointerType::get(
      llvm::IntegerType::get(codegenContext.context(), 8), 0);
  auto i32Ty = llvm::Type::getInt32Ty(codegenContext.context());
  auto TD = llvm::StructType::create(codegenContext.context(), "TypeDesc");
  TD->setBody({i8Ptr, i32Ty}, /*isPacked=*/false);
  return TD;
}

llvm::StructType *makeValueType(CodegenContext &codegenContext,
                                llvm::StructType *TypeDescTy) {
  auto TDPtr = llvm::PointerType::get(TypeDescTy, 0);
  auto payload =
      llvm::ArrayType::get(llvm::Type::getInt8Ty(codegenContext.context()), 8);
  auto ValTy = llvm::StructType::create(codegenContext.context(), "Value");
  ValTy->setBody({TDPtr, payload}, /*isPacked=*/false);
  return ValTy;
}

llvm::StructType *makeConsType(CodegenContext &codegenContext) {
  auto valueTy = codegenContext.getValueTy();
  return llvm::StructType::create(codegenContext.context(), {valueTy, valueTy},
                                  "Cons");
}

void createBuiltinTypeDescs(CodegenContext &codegenContext) {
  auto TypeDescTy = codegenContext.getTypeDescTy();
  auto i32Ty = llvm::Type::getInt32Ty(codegenContext.context());

  auto mkDesc = [&](llvm::StringRef name, int kind) -> llvm::GlobalVariable * {
    // name string constant
    auto nameConst =
        llvm::ConstantDataArray::getString(codegenContext.context(), name);
    auto nameGV = new llvm::GlobalVariable(
        codegenContext.module(), nameConst->getType(), true,
        llvm::GlobalValue::PrivateLinkage, nameConst, name + ".str");
    // ptr to name
    auto zero = llvm::ConstantInt::get(i32Ty, 0);
    llvm::SmallVector<llvm::Constant *, 2> idxs = {zero, zero};
    auto namePtr = llvm::ConstantExpr::getInBoundsGetElementPtr(
        nameConst->getType(), nameGV, idxs);
    // struct initializer { i8* namePtr, i32 kind }
    auto init = llvm::ConstantStruct::get(
        TypeDescTy, {namePtr, llvm::ConstantInt::get(i32Ty, kind)});
    // the global TypeDesc
    return new llvm::GlobalVariable(codegenContext.module(), TypeDescTy, true,
                                    llvm::GlobalValue::ExternalLinkage, init,
                                    "type." + name);
  };

  codegenContext.addType("Int", mkDesc("Int", 0));
  codegenContext.addType("Cons", mkDesc("Cons", 1));
}
