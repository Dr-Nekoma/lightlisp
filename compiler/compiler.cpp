#include "Optimizer.h"
#include "util.h"

llvm::Value *Number::codegen(CodegenContext &codegenContext) {
  auto &B = codegenContext.builder();
  llvm::Value *raw = B.getInt64(value_);

  llvm::Function *boxFn = codegenContext.getFn("boxInt");

  auto name = "num" + std::to_string(value_);
  llvm::Value *boxed = B.CreateCall(boxFn, {raw}, name);

  return boxed;
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
  llvm::Function *CalleeF = codegenContext.getFn(callee_);
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
  auto ptrType = codegenContext.getPtrType();
  std::vector<llvm::Type *> types(args_.size(), ptrType);
  llvm::FunctionType *FT = llvm::FunctionType::get(ptrType, types, false);

  llvm::Function *F = llvm::Function::Create(
      FT, llvm::Function::ExternalLinkage, name_, codegenContext.module());

  // Set names for all arguments.
  unsigned Idx = 0;
  for (auto &arg : F->args())
    arg.setName(args_[Idx++]);

  return F;
}

llvm::Function *Function::codegen(CodegenContext &codegenContext) {
  // First, check for an existing function from a previous 'extern' declaration.
  llvm::Function *currentFn =
      codegenContext.module().getFunction(proto_.getName());

  if (!currentFn)
    currentFn = proto_.codegen(codegenContext);

  if (!currentFn)
    return nullptr;

  if (!currentFn->empty())
    throw std::runtime_error("Function cannot be redefined.");

  // Create a new basic block to start insertion into.
  llvm::BasicBlock *BB =
      llvm::BasicBlock::Create(codegenContext.context(), "entry", currentFn);
  codegenContext.builder().SetInsertPoint(BB);

  codegenContext.named_values().clear();
  for (auto &Arg : currentFn->args()) {
    llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(
        codegenContext, currentFn, codegenContext.getPtrType(),
        std::string(Arg.getName()));

    codegenContext.builder().CreateStore(&Arg, Alloca);

    // 3) register it in the symbol table
    codegenContext.named_values()[std::string(Arg.getName())] = Alloca;
  }

  if (llvm::Value *RetVal = body_->codegen(codegenContext)) {
    codegenContext.builder().CreateRet(RetVal);

    llvm::verifyFunction(*currentFn);

    Optimizer::getFPM().run(*currentFn, Optimizer::getFAM());

    return currentFn;
  }

  // Error reading body, remove function.
  currentFn->eraseFromParent();
  return nullptr;
}

llvm::Value *BuiltInOp::codegen(CodegenContext &codegenContext) {
  // Special case '=' because we don't want to emit the LHS as an expression.
  if (name_ == "setq") {
    // This assume we're building without RTTI because LLVM builds that way by
    // default. If you build LLVM with RTTI this can be changed to a
    // dynamic_cast for automatic error checking.
    auto LHSE = static_cast<Variable *>(fst_.get());
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
  return nullptr;
}

llvm::Value *If::codegen(CodegenContext &codegenContext) {
  auto &builder = codegenContext.builder();
  auto &context = codegenContext.context();

  // 1) Generate the boxed Value* for the condition
  llvm::Value *condBoxed = Cond->codegen(codegenContext);
  if (!condBoxed)
    return nullptr;

  llvm::Function *unboxFn = codegenContext.getFn("unboxInt");
  auto rawCond = builder.CreateCall(unboxFn, {condBoxed}, "unboxedCond");

  // 3) Compare i64 != 0 → i1
  llvm::Value *condI1 =
      builder.CreateICmpNE(rawCond, builder.getInt64(0), "ifcond");

  llvm::Function *currentFn = builder.GetInsertBlock()->getParent();
  llvm::BasicBlock *ThenBB =
      llvm::BasicBlock::Create(context, "then", currentFn);
  llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(context, "else");
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(context, "ifcont");

  builder.CreateCondBr(condI1, ThenBB, ElseBB);

  builder.SetInsertPoint(ThenBB);

  llvm::Value *ThenV = Then->codegen(codegenContext);
  if (!ThenV)
    return nullptr;

  builder.CreateBr(MergeBB);
  // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
  ThenBB = builder.GetInsertBlock();

  // Emit else block.
  currentFn->insert(currentFn->end(), ElseBB);
  builder.SetInsertPoint(ElseBB);

  llvm::Value *ElseV = Else->codegen(codegenContext);
  if (!ElseV)
    return nullptr;

  builder.CreateBr(MergeBB);
  // codegen of 'Else' can change the current block, update ElseBB for the PHI.
  ElseBB = builder.GetInsertBlock();

  currentFn->insert(currentFn->end(), MergeBB);
  builder.SetInsertPoint(MergeBB);
  llvm::PHINode *PN =
      builder.CreatePHI(codegenContext.getPtrType(), 2, "iftmp");

  PN->addIncoming(ThenV, ThenBB);
  PN->addIncoming(ElseV, ElseBB);
  return PN;
}

llvm::Value *Goto::codegen(CodegenContext &codegenContext) {
  llvm::Function *currentFn =
      codegenContext.builder().GetInsertBlock()->getParent();
  auto &builder = codegenContext.builder();

  auto lastVal = CreateEntryBlockAlloca(
      codegenContext, currentFn, codegenContext.getPtrType(), "tagbody.ret");

  codegenContext.tagEnvs().emplace_back();

  for (auto &item : body_) {
    if (auto tag =
            std::get_if<std::string>(&item)) { // check for the repeating tags
      codegenContext.lastTagEnv()[*tag] =
          llvm::BasicBlock::Create(codegenContext.context(), *tag, currentFn);
    }
  }

  llvm::BasicBlock *afterBB = llvm::BasicBlock::Create(
      codegenContext.context(),
      "tagbody.exit" + std::to_string(codegenContext.tagEnvs().size()),
      currentFn);

  llvm::BasicBlock *curBB = builder.GetInsertBlock();

  for (auto &item : body_) {
    if (auto tag = std::get_if<std::string>(&item)) {
      if (!curBB->getTerminator())
        builder.CreateBr(codegenContext.lastTagEnv()[*tag]);
      builder.SetInsertPoint(codegenContext.lastTagEnv()[*tag]);
    } else {
      auto &expr = std::get<ObjPtr>(item);
      auto v = expr->codegen(codegenContext);
      if (v)
        builder.CreateStore(v, lastVal);
      curBB = builder.GetInsertBlock();
    }
  }

  if (!curBB->getTerminator())
    builder.CreateBr(afterBB);
  builder.SetInsertPoint(afterBB);
  llvm::Value *last =
      builder.CreateLoad(codegenContext.getPtrType(), lastVal, "tagbody.last");
  codegenContext.tagEnvs().pop_back();

  return last;
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

  return nullptr;
}

/*
llvm::Value *VarExprAST::codegen(CodegenContext &codegenContext) {
  std::vector<llvm::AllocaInst *> OldBindings;

  llvm::Function *currentFn =
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

    llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(currentFn, VarName);
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