#include "Optimizer.h"
#include <llvm/CodeGen/Passes.h>

llvm::FunctionPassManager &Optimizer::getFPM() {
  static std::unique_ptr<llvm::FunctionPassManager> FAM =
      std::make_unique<llvm::FunctionPassManager>();
  return *FAM;
}

llvm::LoopAnalysisManager &Optimizer::getLAM() {
  static std::unique_ptr<llvm::LoopAnalysisManager> LAM =
      std::make_unique<llvm::LoopAnalysisManager>();
  return *LAM;
}

llvm::FunctionAnalysisManager &Optimizer::getFAM() {
  static std::unique_ptr<llvm::FunctionAnalysisManager> FAM =
      std::make_unique<llvm::FunctionAnalysisManager>();
  return *FAM;
}

llvm::CGSCCAnalysisManager &Optimizer::getCGAM() {
  static std::unique_ptr<llvm::CGSCCAnalysisManager> CGAM =
      std::make_unique<llvm::CGSCCAnalysisManager>();
  return *CGAM;
}

llvm::ModuleAnalysisManager &Optimizer::getMAM() {
  static std::unique_ptr<llvm::ModuleAnalysisManager> MAM =
      std::make_unique<llvm::ModuleAnalysisManager>();
  return *MAM;
}
llvm::PassInstrumentationCallbacks &Optimizer::getPIC() {
  static std::unique_ptr<llvm::PassInstrumentationCallbacks> PIC =
      std::make_unique<llvm::PassInstrumentationCallbacks>();
  return *PIC;
}
llvm::StandardInstrumentations &
Optimizer::getSI(CodegenContext &codegenContext) {
  static std::unique_ptr<llvm::StandardInstrumentations> SI =
      std::make_unique<llvm::StandardInstrumentations>(
          codegenContext.context.context,
          /*DebugLogging*/ true);
  return *SI;
}

void InitializeModuleAndManagers(CodegenContext &codegenContext) {
  Optimizer::getSI(codegenContext)
      .registerCallbacks(Optimizer::getPIC(), &Optimizer::getMAM());

  // Do simple "peephole" optimizations and bit-twiddling optimizations.
  // Optimizer::getFPM().addPass(llvm::InstCombinePass());

  // Eliminate Common SubExpressions.
  Optimizer::getFPM().addPass(llvm::GVNPass());

  // Simplify the control flow graph (deleting unreachable blocks, etc).
  Optimizer::getFPM().addPass(llvm::SimplifyCFGPass());

  // Promote allocas to registers.
  Optimizer::getFPM().addPass(llvm::PromotePass());

  // Do simple "peephole" optimizations and bit-twiddling optimizations (again).
  Optimizer::getFPM().addPass(
      llvm::InstCombinePass()); // Changed from InstructionCombiningPass

  // Reassociate expressions (again).
  Optimizer::getFPM().addPass(llvm::ReassociatePass());

  // Register analysis passes used in these transform passes.
  llvm::PassBuilder PB;
  PB.registerModuleAnalyses(Optimizer::getMAM());
  PB.registerFunctionAnalyses(Optimizer::getFAM());
  PB.crossRegisterProxies(Optimizer::getLAM(), Optimizer::getFAM(),
                          Optimizer::getCGAM(), Optimizer::getMAM());
}