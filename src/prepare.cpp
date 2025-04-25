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