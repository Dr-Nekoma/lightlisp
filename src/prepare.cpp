#include "prepare.h"

std::vector<std::unique_ptr<Function>>
prepareTopLevelFns(CodegenContext &codegenContext, Parser &&parser) {
  std::vector<std::unique_ptr<Function>> functions;

  while (!parser.IsEnd()) {
    auto syntax = parser.Read();
    if (!syntax)
      continue;
    auto ast = ir1LispTransform(std::move(syntax));

    auto fnAST = dynamic_cast<Function *>(ast.release());
    if (!fnAST)
      throw std::runtime_error(
          "Only function definitions allowed at top level");

    functions.emplace_back(std::make_unique<Function>(std::move(*fnAST)));
    llvm::errs() << functions.back()->getProto().getName() << '\n';
  }

  for (auto &Fptr : functions) {
    if (!Fptr->codegen(codegenContext))
      throw std::runtime_error("Codegen failed for a function");
  }
  return functions;
}

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
  auto retI64 = unboxIntVal(codegenContext, boxedRet);

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

void emitBuiltIn(CodegenContext &codegenContext, std::string &&fnName,
                 IntOpFn opFn) {
  auto &builder = codegenContext.builder();
  auto ptrTy = codegenContext.getPtrType();
  auto internalfnName = codegenContext.transformName(fnName);
  // -- declare: Value* @fnName(Value*,Value*)
  llvm::FunctionType *FT =
      llvm::FunctionType::get(ptrTy, {ptrTy, ptrTy}, /*vararg=*/false);
  llvm::Function *F =
      llvm::Function::Create(FT, llvm::Function::ExternalLinkage,
                             internalfnName, codegenContext.module());
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
  llvm::Value *lhs = unboxIntVal(codegenContext, &*F->args().begin()); // x0
  llvm::Value *rhs =
      unboxIntVal(codegenContext, &*std::next(F->args().begin())); // x1

  // apply the integer op
  llvm::Value *resI64 = opFn(builder, lhs, rhs);

  auto boxed = boxIntVal(codegenContext, resI64, "ret." + fnName);
  // return it
  builder.CreateRet(boxed);
}

void initBuiltIns(CodegenContext &codegenContext) {
  emitBuiltIn(
      codegenContext, "+",
      [](llvm::IRBuilder<> &builder, llvm::Value *fst, llvm::Value *snd) {
        return builder.CreateAdd(fst, snd, "addtmp");
      });
  emitBuiltIn(
      codegenContext, "-",
      [](llvm::IRBuilder<> &builder, llvm::Value *fst, llvm::Value *snd) {
        return builder.CreateSub(fst, snd, "subtmp");
      });
  emitBuiltIn(
      codegenContext, "*",
      [](llvm::IRBuilder<> &builder, llvm::Value *fst, llvm::Value *snd) {
        return builder.CreateMul(fst, snd, "multmp");
      });
  emitBuiltIn(
      codegenContext, "<",
      [](llvm::IRBuilder<> &builder, llvm::Value *fst, llvm::Value *snd) {
        auto boolRes = builder.CreateICmpSLT(fst, snd, "cmptmp");
        return builder.CreateZExt(boolRes, builder.getInt64Ty(), "booltoint");
      });
}