#include "prepare.h"

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
  if (auto F = dynamic_cast<Function *>(body.get())) {
    return body;
  }

  // Otherwise wrap it in an __anon_expr
  auto Proto =
      std::make_unique<Prototype>("__anon_expr", std::vector<std::string>());
  return std::make_unique<Function>(std::move(Proto), std::move(body));
}*/

void initTypes(CodegenContext &codegenContext) {
  createBuiltinTypeDescs(codegenContext);
}

int main() { // Needs cleanup
  std::ifstream my_lisp("lisp.txt");
  if (!my_lisp) {
    llvm::errs() << "Error: cannot open lisp.txt\n";
    return 1;
  }
  Tokenizer tok(&my_lisp);
  Parser parser(std::move(tok));

  CodegenContext codegenContext;
  InitializeModuleAndManagers(codegenContext);

  initTypes(codegenContext);
  std::vector<std::unique_ptr<Function>> functions =
      prepareTopLevelFns(codegenContext, std::move(parser));

  auto lispMain = codegenContext.module().getFunction("main");
  if (!lispMain)
    throw std::runtime_error("`main` function not found in Lisp code");
  lispMain->setName("lisp_main");
  {
    auto &C = codegenContext.context();
    auto &builder = codegenContext.builder();
    auto &M = codegenContext.module();

    // 1) Make the Function and its prototype
    auto FT = llvm::FunctionType::get(builder.getInt32Ty(), {}, false);
    auto wrapper =
        llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "main", &M);

    // 2) Create three blocks: entry, body, exit
    auto entryBB = llvm::BasicBlock::Create(C, "entry", wrapper);
    auto bodyBB = llvm::BasicBlock::Create(C, "body", wrapper);
    auto exitBB = llvm::BasicBlock::Create(C, "exit", wrapper);
    // ——— ENTRY block — mmap and jump to body ———
    builder.SetInsertPoint(entryBB);
    prepareArena(codegenContext);

    // jump into the body
    builder.CreateBr(bodyBB);

    // ——— BODY block — call lisp_main & branch to exit ———
    builder.SetInsertPoint(bodyBB);
    // call your generated lisp_main (returns Value*)

    auto ret32 = prepareCMain(codegenContext, lispMain);

    // stash the return code in a spill slot (or pass it via a reg)
    // for simplicity, we can branch with an i32 constant:
    // but LLVM IR requires us to ret in one place, so:
    builder.CreateBr(exitBB);
    // remember ret32 in a PHI or alloca if needed…
    // simplest is to alloca a temp at entry, store ret32, then reload below.

    // ——— EXIT block — munmap and RET ———
    builder.SetInsertPoint(exitBB);
    // reload ret32 from that temp or compute inline
    // (if your ABI lets you pass it in %eax/%edi you can omit this
    // store+load)

    // munmap(arenaBase, arenaSize)
    munmapArena(codegenContext);

    // finally return the i32
    builder.CreateRet(ret32 /* or reload from alloca */);
  }

  for (auto &F : codegenContext.module()) {
    llvm::verifyFunction(F);
  }

  if (verifyModule(codegenContext.module(), &llvm::errs())) {
    codegenContext.module().print(llvm::errs(), nullptr);
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