#include "parser.h"
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

  codegenContext.context.module.setDataLayout(
      TargetMachine->createDataLayout());
  codegenContext.context.module.setTargetTriple(TargetTriple);

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

  pass.run(codegenContext.context.module);
  dest.flush();
  return 0;
}

static std::vector<std::unique_ptr<Function>>
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
  }

  for (auto &Fptr : functions) {
    if (!Fptr->codegen(codegenContext))
      throw std::runtime_error("Codegen failed for a function");
  }
  return functions;
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

int main(int argc, char **argv) { // Needs cleanup
  if (argc < 2) {
    llvm::errs() << "Usage: " << argv[0] << " <input-file>\n";
    return 1;
  }
  std::string filename = argv[1];

  std::ifstream my_lisp(filename);
  if (!my_lisp) {
    llvm::errs() << "Error: cannot open '" << filename << "'\n";
    return 1;
  }

  Tokenizer tok(&my_lisp);
  Parser parser(std::move(tok));

  CodegenContext codegenContext;
  InitializeModuleAndManagers(codegenContext);

  std::vector<std::unique_ptr<Function>> functions =
      prepareTopLevelFns(codegenContext, std::move(parser));
  codegenContext.lexenv.initGlobalCtors();
  codegenContext.emitCtors();
  // auto lispMain = codegenContext.context.module.getFunction("main");
  // if (!lispMain)
  //  throw std::runtime_error("`main` function not found in Lisp code");
  // lispMain->setName("lisp_main");
  {
    auto &C = codegenContext.context.context;
    auto &builder = codegenContext.context.builder;
    auto &M = codegenContext.context.module;

    // 1) Make the Function and its prototype
    auto FT = llvm::FunctionType::get(builder.getInt32Ty(), {}, false);
    auto wrapper =
        llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "main", &M);

    // 2) Create three blocks: entry, body, exit
    auto bodyBB = llvm::BasicBlock::Create(C, "body", wrapper);
    auto exitBB = llvm::BasicBlock::Create(C, "exit", wrapper);

    // ——— BODY block — call lisp_main & branch to exit ———
    builder.SetInsertPoint(bodyBB);
    // call your generated lisp_main (returns Value*)

    auto ret32 = prepareCMain(codegenContext);

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
    codegenContext.memory_manager.munmapArena(codegenContext.context);

    // finally return the i32

    // auto loaded =
    //     builder.CreateLoad(builder.getInt32Ty(), codegenContext.debug);
    builder.CreateRet(ret32);
  }

  for (auto &F : codegenContext.context.module) {
    llvm::verifyFunction(F);
  }

  if (verifyModule(codegenContext.context.module, &llvm::errs())) {
    codegenContext.context.module.print(llvm::errs(), nullptr);
  }

  std::error_code EC;
  llvm::raw_fd_ostream llOut(filename + ".ll", EC, llvm::sys::fs::OF_Text);
  if (EC)
    llvm::errs() << "Could not open lisp.ll for writing\n";
  else
    codegenContext.context.module.print(llOut, nullptr);

  int err = genObjectFile(codegenContext);
  return err != 0 ? 1 : 0;
}