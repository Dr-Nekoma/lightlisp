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
  std::vector<llvm::Type *> types(
      args_.size(), llvm::Type::getInt64Ty(codegenContext.context()));
  llvm::FunctionType *FT = llvm::FunctionType::get(
      llvm::Type::getInt64Ty(codegenContext.context()), types, false);

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
  return TmpB.CreateAlloca(llvm::Type::getInt64Ty(codegenContext.context()),
                           nullptr, VarName);
}

llvm::Function *Function::codegen(CodegenContext &codegenContext) {
  // First, check for an existing function from a previous 'extern' declaration.
  llvm::Function *TheFunction =
      codegenContext.module().getFunction(proto_.getName());

  if (!TheFunction)
    TheFunction = proto_.codegen(codegenContext);

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
    auto *LHSE = static_cast<Variable *>(fst_.get());
    if (!LHSE)
      throw std::runtime_error("destination of '=' must be a variable");

    // Codegen the RHS.
    llvm::Value *Val = snd_->codegen(codegenContext);
    if (!Val)
      return nullptr;

    // Look up the name.
    llvm::Value *Variable = codegenContext.named_values()[LHSE->getName()];
    if (!Variable)
      throw std::runtime_error("Unknown variable name");

    codegenContext.builder().CreateStore(Val, Variable);
    return Val;
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

  auto *zero =
      llvm::ConstantInt::get(llvm::Type::getInt1Ty(codegenContext.context()),
                             0); // there is CreateCondBr

  CondV = codegenContext.builder().CreateICmpNE(CondV, zero, "ifcond");

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
      llvm::Type::getInt64Ty(codegenContext.context()), 2, "iftmp");

  PN->addIncoming(ThenV, ThenBB);
  PN->addIncoming(ElseV, ElseBB);
  return PN;
}

llvm::Value *Goto::codegen(CodegenContext &codegenContext) {
  llvm::Function *TheFunction =
      codegenContext.builder().GetInsertBlock()->getParent();

  codegenContext.tagEnvs().emplace_back();

  for (auto &item : body_) {
    if (auto *tag =
            std::get_if<std::string>(&item)) { // check for the repeating tags
      codegenContext.lastTagEnv()[*tag] =
          llvm::BasicBlock::Create(codegenContext.context(), *tag, TheFunction);
    }
  }

  llvm::BasicBlock *afterBB = llvm::BasicBlock::Create(
      codegenContext.context(), "tagbody.exit", TheFunction);

  llvm::Value *retVal = nullptr;

  llvm::BasicBlock *curBB = codegenContext.builder().GetInsertBlock();

  for (auto &item : body_) {
    if (auto *tag = std::get_if<std::string>(&item)) {
      if (!curBB->getTerminator())
        codegenContext.builder().CreateBr(codegenContext.lastTagEnv()[*tag]);
      codegenContext.builder().SetInsertPoint(
          codegenContext.lastTagEnv()[*tag]);
    } else {
      auto &expr = std::get<ObjPtr>(item);
      auto v = expr->codegen(codegenContext);
      if (v)
        retVal = v;
      curBB = codegenContext.builder().GetInsertBlock();
    }
  }

  if (!curBB->getTerminator())
    codegenContext.builder().CreateBr(afterBB);
  codegenContext.builder().SetInsertPoint(afterBB);

  codegenContext.tagEnvs().pop_back();

  return retVal;
}

llvm::Value *Go::codegen(CodegenContext &codegenContext) {
  llvm::BasicBlock *dest = nullptr;
  for (auto env = codegenContext.tagEnvs().rbegin();
       env != codegenContext.tagEnvs().rend(); ++env) {
    auto it = env->find(tag_);
    if (it != env->end()) {
      dest = it->second;
      break;
    }
  }

  if (!dest)
    throw std::runtime_error("Undefined tag in go: " + tag_);

  codegenContext.builder().CreateBr(dest);

  return llvm::ConstantInt::get(
      llvm::Type::getInt64Ty(codegenContext.context()), 0);
}

/*
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