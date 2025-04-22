#include "../definitions/interfaces.h"
#include "../ir1lisp/ir1lisp.h"
#include "../scheme-parser/parser.h"
#include "../scheme-tokenizer/tokenizer.h"
#include "Optimizer.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/MC/TargetRegistry.h"
#include <llvm/Support/FileSystem.h>
#include <llvm/TargetParser/Host.h>

#include <fstream>

int genObjectFile(CodegenContext &codegenContext) {
  auto TargetTriple = llvm::sys::getDefaultTargetTriple();

  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();

  std::string Error;
  auto Target = llvm::TargetRegistry::lookupTarget(TargetTriple, Error);

  // Print an error and exit if we couldn't find the requested target.
  // This generally occurs if we've forgotten to initialise the
  // TargetRegistry or we have a bogus target triple.
  if (!Target) {
    llvm::errs() << Error;
    return 1;
  }

  auto CPU = "generic";
  auto Features = "";

  llvm::TargetOptions opt;
  auto TargetMachine = Target->createTargetMachine(TargetTriple, CPU, Features,
                                                   opt, llvm::Reloc::PIC_);

  codegenContext.module().setDataLayout(TargetMachine->createDataLayout());
  codegenContext.module().setTargetTriple(TargetTriple);

  auto Filename = "lisp.o";
  std::error_code EC;
  llvm::raw_fd_ostream dest(Filename, EC, llvm::sys::fs::OF_None);

  if (EC) {
    llvm::errs() << "Could not open file: " << EC.message();
    return 1;
  }

  llvm::legacy::PassManager pass;
  auto FileType = llvm::CodeGenFileType::ObjectFile;

  if (TargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
    llvm::errs() << "TargetMachine can't emit a file of this type";
    return 1;
  }

  pass.run(codegenContext.module());
  dest.flush();
  return 0;
}

/*ObjPtr parseTopLevelExpr(ObjPtr body) {
  if (auto *F = dynamic_cast<Function *>(body.get())) {
    return body;
  }

  // Otherwise wrap it in an __anon_expr
  auto Proto =
      std::make_unique<Prototype>("__anon_expr", std::vector<std::string>());
  return std::make_unique<Function>(std::move(Proto), std::move(body));
}*/

int main() {
  std::ifstream my_lisp("lisp.txt");
  if (!my_lisp) {
    llvm::errs() << "Error: cannot open lisp.txt\n";
    return 1;
  }
  Tokenizer tok(&my_lisp);
  Parser parser(std::move(tok));

  CodegenContext codegenContext;
  InitializeModuleAndManagers(codegenContext);

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

  auto *lispMain = codegenContext.module().getFunction("main");
  if (!lispMain)
    throw std::runtime_error("`main` function not found in Lisp code");
  lispMain->setName("lisp_main");
  {
    auto &C = codegenContext.context();
    auto &builder = codegenContext.builder();

    auto *FT = llvm::FunctionType::get(llvm::Type::getInt32Ty(C), false);
    auto *wrapper = llvm::Function::Create(FT, llvm::Function::ExternalLinkage,
                                           "main", &codegenContext.module());

    auto *BB = llvm::BasicBlock::Create(C, "entry", wrapper);
    builder.SetInsertPoint(BB);

    auto *ret64 = builder.CreateCall(lispMain, {}, "call_lisp_main");
    auto *ret32 =
        builder.CreateTrunc(ret64, llvm::Type::getInt32Ty(C), "ret32");
    builder.CreateRet(ret32);
  }

  for (auto &F : codegenContext.module()) {
    llvm::verifyFunction(F);
  }

  std::error_code EC;
  llvm::raw_fd_ostream llOut("lisp.ll", EC, llvm::sys::fs::OF_Text);
  if (EC)
    llvm::errs() << "Could not open lisp.ll for writing\n";
  else
    codegenContext.module().print(llOut, nullptr);

  int err = genObjectFile(codegenContext);
  return err != 0 ? 1 : 0;
}