#include "../definitions/interfaces.h"

llvm::Value *Number::codegen(CodegenContext &codegenContext) {
  return llvm::ConstantInt::get(codegenContext.context(),
                                llvm::APInt(64, value_, true));
}

llvm::Value *Variable::codegen(CodegenContext &codegenContext) {
  llvm::AllocaInst *V = codegenContext.named_values()[name_];
  if (!V)
    throw std::runtime_error("Unknown variable name");

  return codegenContext.builder().CreateLoad(V->getAllocatedType(), V,
                                             name_.c_str());
}

llvm::Value *Call::codegen(CodegenContext &codegenContext) {
  // Look up the name in the global module table.
  llvm::Function *CalleeF = codegenContext.module().getFunction(callee_);
  if (!CalleeF)
    throw std::runtime_error("Unknown function referenced");

  // If argument mismatch error.
  if (CalleeF->arg_size() != args_.size())
    throw std::runtime_error("Incorrect # arguments passed");

  std::vector<llvm::Value *> ArgsV;
  for (auto &Arg : args_) {
    ArgsV.push_back(Arg->codegen(codegenContext));
    if (!ArgsV.back())
      return nullptr;
  }

  return codegenContext.builder().CreateCall(CalleeF, ArgsV, "calltmp");
}

llvm::Function *Prototype::codegen(CodegenContext &codegenContext) {
  // Make the function type:  double(double,double) etc.
  std::vector<llvm::Type *> Doubles(
      args_.size(), llvm::Type::getDoubleTy(codegenContext.context()));
  llvm::FunctionType *FT = llvm::FunctionType::get(
      llvm::Type::getDoubleTy(codegenContext.context()), Doubles, false);

  llvm::Function *F = llvm::Function::Create(
      FT, llvm::Function::ExternalLinkage, name_, codegenContext.module());

  // Set names for all arguments.
  unsigned Idx = 0;
  for (auto &arg : F->args())
    arg.setName(args_[Idx++]);

  return F;
}

/// CreateEntryBlockAlloca - Create an alloca instruction in the entry block
/// of the function.  This is used for mutable variables etc.
static llvm::AllocaInst *CreateEntryBlockAlloca(CodegenContext &codegenContext,
                                                llvm::Function *TheFunction,
                                                const std::string &VarName) {
  llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                         TheFunction->getEntryBlock().begin());
  return TmpB.CreateAlloca(llvm::Type::getDoubleTy(codegenContext.context()),
                           nullptr, VarName);
}

llvm::Function *Function::codegen(CodegenContext &codegenContext) {
  // First, check for an existing function from a previous 'extern' declaration.
  llvm::Function *TheFunction =
      codegenContext.module().getFunction(proto_->getName());

  if (!TheFunction)
    TheFunction = proto_->codegen(codegenContext);

  if (!TheFunction)
    return nullptr;

  if (!TheFunction->empty())
    throw std::runtime_error("Function cannot be redefined.");

  // Create a new basic block to start insertion into.
  llvm::BasicBlock *BB =
      llvm::BasicBlock::Create(codegenContext.context(), "entry", TheFunction);
  codegenContext.builder().SetInsertPoint(BB);

  // Record the function arguments in the named_values map.
  codegenContext.named_values().clear();
  for (auto &Arg : TheFunction->args()) {
    // Create an alloca for this variable.
    llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(
        codegenContext, TheFunction, std::string(Arg.getName()));

    // Store the initial value into the alloca.
    codegenContext.builder().CreateStore(&Arg, Alloca);

    // Add arguments to variable symbol table.
    codegenContext.named_values()[std::string(Arg.getName())] = Alloca;
  }

  if (llvm::Value *RetVal = body_->codegen(codegenContext)) {
    // Finish off the function.
    codegenContext.builder().CreateRet(RetVal);

    // Validate the generated code, checking for consistency.
    llvm::verifyFunction(*TheFunction);

    // Optimizer::getFPM().run(*TheFunction, Optimizer::getFAM());

    return TheFunction;
  }

  // Error reading body, remove function.
  TheFunction->eraseFromParent();
  return nullptr;
}

llvm::Value *BuiltInOp::codegen(CodegenContext &codegenContext) {
  // Special case '=' because we don't want to emit the LHS as an expression.
  if (name_ == "setq") {
    // This assume we're building without RTTI because LLVM builds that way by
    // default. If you build LLVM with RTTI this can be changed to a
    // dynamic_cast for automatic error checking.
    /*auto *LHSE = static_cast<Variable *>(LHS.get());
    if (!LHSE)
      return LogErrorV("destination of '=' must be a variable");

    // Codegen the RHS.
    llvm::Value *Val = RHS->codegen();
    if (!Val)
      return nullptr;

    // Look up the name.
    llvm::Value *Variable = getNamedValues()[LHSE->getName()];
    if (!Variable)
      return LogErrorV("Unknown variable name");

    getBuilder().CreateStore(Val, Variable);
    return Val;*/
    throw std::runtime_error("Unimplemented setq assignment");
  }

  llvm::Value *fst = fst_->codegen(codegenContext);
  llvm::Value *snd = snd_->codegen(codegenContext);
  if (!fst || !snd) // a hack
    return nullptr;

  if (name_ == "+") {
    return codegenContext.builder().CreateAdd(fst, snd, "addtmp");
  } else if (name_ == "-") {
    return codegenContext.builder().CreateSub(fst, snd, "subtmp");
  } else if (name_ == "*") {
    return codegenContext.builder().CreateMul(fst, snd, "multmp");
  } else if (name_ == "<") {
    auto res = codegenContext.builder().CreateICmpSLT(fst, snd, "cmptmp");
    return res;
  } else {
    throw std::runtime_error("Non existent operator");
  }
}

llvm::Value *If::codegen(CodegenContext &codegenContext) {
  llvm::Value *CondV = Cond->codegen(codegenContext);
  if (!CondV)
    return nullptr;

  CondV = codegenContext.builder().CreateFCmpONE(
      CondV,
      llvm::ConstantFP::get(codegenContext.context(), llvm::APFloat(0.0)),
      "ifcond");

  llvm::Function *ThisFunction =
      codegenContext.builder().GetInsertBlock()->getParent();
  llvm::BasicBlock *ThenBB =
      llvm::BasicBlock::Create(codegenContext.context(), "then", ThisFunction);
  llvm::BasicBlock *ElseBB =
      llvm::BasicBlock::Create(codegenContext.context(), "else");
  llvm::BasicBlock *MergeBB =
      llvm::BasicBlock::Create(codegenContext.context(), "ifcont");

  codegenContext.builder().CreateCondBr(CondV, ThenBB, ElseBB);

  codegenContext.builder().SetInsertPoint(ThenBB);

  llvm::Value *ThenV = Then->codegen(codegenContext);
  if (!ThenV)
    return nullptr;

  codegenContext.builder().CreateBr(MergeBB);
  // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
  ThenBB = codegenContext.builder().GetInsertBlock();

  // Emit else block.
  ThisFunction->insert(ThisFunction->end(), ElseBB);
  codegenContext.builder().SetInsertPoint(ElseBB);

  llvm::Value *ElseV = Else->codegen(codegenContext);
  if (!ElseV)
    return nullptr;

  codegenContext.builder().CreateBr(MergeBB);
  // codegen of 'Else' can change the current block, update ElseBB for the PHI.
  ElseBB = codegenContext.builder().GetInsertBlock();

  ThisFunction->insert(ThisFunction->end(), MergeBB);
  codegenContext.builder().SetInsertPoint(MergeBB);
  llvm::PHINode *PN = codegenContext.builder().CreatePHI(
      llvm::Type::getDoubleTy(codegenContext.context()), 2, "iftmp");

  PN->addIncoming(ThenV, ThenBB);
  PN->addIncoming(ElseV, ElseBB);
  return PN;
}
/*
llvm::Value *ForExprAST::codegen(CodegenContext &codegenContext) {
  // Make the new basic block for the loop header, inserting after current
  // block.
  llvm::Function *TheFunction =
      codegenContext.builder().GetInsertBlock()->getParent();
  // Create an alloca for the variable in the entry block.
  llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);

  llvm::BasicBlock *LoopBB =
      llvm::BasicBlock::Create(codegenContext.context(), "loop", TheFunction);

  // Emit the start code first, without 'variable' in scope.
  llvm::Value *StartVal = Start->codegen(CodegenContext & CodegenContext);
  if (!StartVal)
    return nullptr;
  codegenContext.builder().CreateStore(StartVal, Alloca);

  // Insert an explicit fall through from the current block to the LoopBB.
  codegenContext.builder().CreateBr(LoopBB);

  // Start insertion in LoopBB.
  codegenContext.builder().SetInsertPoint(LoopBB);

  // Within the loop, the variable is defined equal to the PHI node.  If it
  // shadows an existing variable, we have to restore it, so save it now.
  llvm::AllocaInst *OldVal = codegenContext.named_values()[VarName];
  codegenContext.named_values()[VarName] = Alloca;

  // Emit the body of the loop.  This, like any other expr, can change the
  // current BB.  Note that we ignore the value computed by the body, but don't
  // allow an error.
  if (!Body->codegen(CodegenContext & CodegenContext))
    return nullptr;

  // Emit the step value.
  llvm::Value *StepVal = nullptr;
  if (Step) {
    StepVal = Step->codegen(CodegenContext & CodegenContext);
    if (!StepVal)
      return nullptr;
  } else {
    // If not specified, use 1.0.
    StepVal =
        llvm::ConstantFP::get(codegenContext.context(), llvm::APFloat(1.0));
  }

  // Compute the end condition.
  llvm::Value *EndCond = End->codegen(CodegenContext & CodegenContext);
  if (!EndCond)
    return nullptr;

  // Reload, increment, and restore the alloca.  This handles the case where
  // the body of the loop mutates the variable.
  llvm::Value *CurVar = codegenContext.builder().CreateLoad(
      Alloca->getAllocatedType(), Alloca, VarName.c_str());
  llvm::Value *NextVar =
      codegenContext.builder().CreateFAdd(CurVar, StepVal, "nextvar");
  codegenContext.builder().CreateStore(NextVar, Alloca);

  // Convert condition to a bool by comparing non-equal to 0.0.
  EndCond = codegenContext.builder().CreateFCmpONE(
      EndCond,
      llvm::ConstantFP::get(codegenContext.context(), llvm::APFloat(0.0)),
      "loopcond");

  // Create the "after loop" block and insert it.
  llvm::BasicBlock *AfterBB = llvm::BasicBlock::Create(
      codegenContext.context(), "afterloop", TheFunction);

  // Insert the conditional branch into the end of LoopEndBB.
  // br i1 %loopcond, label %loop, label %afterloop
  codegenContext.builder().CreateCondBr(EndCond, LoopBB, AfterBB);

  // Any new code will be inserted in AfterBB.
  codegenContext.builder().SetInsertPoint(AfterBB);

  // Restore the unshadowed variable.
  if (OldVal)
    codegenContext.named_values()[VarName] = OldVal;
  else
    codegenContext.named_values().erase(VarName);

  // for expr always returns 0.0.
  return llvm::Constant::getNullValue(
      llvm::Type::getDoubleTy(codegenContext.context()));
}

llvm::Value *VarExprAST::codegen(CodegenContext &codegenContext) {
  std::vector<llvm::AllocaInst *> OldBindings;

  llvm::Function *TheFunction =
      codegenContext.builder().GetInsertBlock()->getParent();

  // Register all variables and emit their initializer.
  for (auto &Var : VarNames) {
    const std::string &VarName = Var.first;
    ExprAST *Init = Var.second.get();

    // Emit the initializer before adding the variable to scope, this prevents
    // the initializer from referencing the variable itself, and permits stuff
    // like this:
    //  var a = 1 in
    //    var a = a in ...   # refers to outer 'a'.
    llvm::Value *InitVal;

    if (Init) {
      InitVal = Init->codegen(CodegenContext & CodegenContext);
      if (!InitVal)
        return nullptr;
    } else { // If not specified, use 0.0.
      InitVal =
          llvm::ConstantFP::get(codegenContext.context(), llvm::APFloat(0.0));
    }

    llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
    codegenContext.builder().CreateStore(InitVal, Alloca);

    // Remember the old variable binding so that we can restore the binding when
    // we unrecurse.
    OldBindings.push_back(codegenContext.named_values()[VarName]);

    // Remember this binding.
    codegenContext.named_values()[VarName] = Alloca;
  }

  // Codegen the body, now that all vars are in scope.
  llvm::Value *BodyVal = Body->codegen(CodegenContext & CodegenContext);
  if (!BodyVal)
    return nullptr;

  // Pop all our variables from scope.
  for (unsigned i = 0, e = VarNames.size(); i != e; ++i)
    codegenContext.named_values()[VarNames[i].first] = OldBindings[i];

  // Return the body computation.
  return BodyVal;
}*/