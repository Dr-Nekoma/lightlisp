#include "Optimizer.h"
#include "meta.h"
#include "objects.h"
#include "util.h"

llvm::Value *Number::codegen(CodegenContext &codegenContext) {
  auto &B = codegenContext.context.builder;
  llvm::Value *raw = B.getInt64(value_);

  auto name = "num" + std::to_string(value_);
  llvm::Value *boxed =
      codegenContext.type_manager.packVal(codegenContext, raw, Type::Int);

  return boxed;
}

llvm::Value *Variable::codegen(CodegenContext &codegenContext) {
  auto [var, is_local] = codegenContext.lexenv.lookUpVar(name_);
  auto &builder = codegenContext.context.builder;

  if (!var.get() && !var.getGlob())
    throw std::runtime_error("Unknown variable name");
  if (!is_local && !var.getGlob()) {
    codegenContext.lexenv.addFreeVar(name_, var.get());

    auto idx =
        codegenContext.lexenv.freeVarsSize() -
        1; // idx after I add the var, so that later when we emit the
           // environment it would have this exact idxx for this exact variable
    auto curEnv = codegenContext.lexenv.getCurrentEnv();

    auto slotPtr = builder.CreateInBoundsGEP(
        codegenContext.type_manager.getEnvTy(), // the Env struct type
        curEnv,                                 // Env*
        {
            builder.getInt32(0),  // index into the pointer itself
            builder.getInt32(1),  // field #1 = slots
            builder.getInt64(idx) // element in the array
        },
        "env.slot.ptr");

    return builder.CreateLoad(codegenContext.type_manager.getPtrType(), slotPtr,
                              name_);
  }
  // if not local, instead of a load need GEP + load
  if (auto local = var.get()) {
    return builder.CreateLoad(local->getAllocatedType(), local, name_);
  }
  return var.getGlob();
}

llvm::Value *Call::codegen(CodegenContext &codegenContext) {
  // Look up the name in the global module table.
  llvm::Function *CalleeF = codegenContext.lexenv.getBuiltInFn(callee_);
  if (CalleeF) {
    // If argument mismatch error.
    if (CalleeF->arg_size() != args_.size())
      throw std::runtime_error("Incorrect # arguments passed");

    std::vector<llvm::Value *> ArgsV;
    for (auto &Arg : args_) {
      ArgsV.push_back(Arg->codegen(codegenContext));
      if (!ArgsV.back())
        return nullptr;
    }

    auto call =
        codegenContext.context.builder.CreateCall(CalleeF, ArgsV, "calltmp");
    if (callee_ == "car" || callee_ == "cdr") { // FIXME this can't be good
      call->setOnlyReadsMemory();
    }

    return call;
  }
  auto userVal = codegenContext.lexenv.lookUpVar(callee_).first;
  if (auto local = userVal.get()) {
    auto closure = codegenContext.type_manager.checkAndUnpack(codegenContext,
                                                              local, Type::Fn);

    return createClosurecall(codegenContext, closure, args_);
  }
  if (auto global = userVal.getGlob()) {
    auto loaded = codegenContext.context.builder.CreateLoad(
        codegenContext.type_manager.getPtrType(), global, "loaded.global");
    auto closure = codegenContext.type_manager.checkAndUnpack(codegenContext,
                                                              loaded, Type::Fn);
    return createClosurecall(codegenContext, closure, args_);
  }
  return nullptr;
}

/*
llvm::Value *Prototype::codegen(CodegenContext &codegenContext) {
  auto ptrType = codegenContext.type_manager.getPtrType();
  std::vector<llvm::Type *> types(args_.size(), ptrType);
  llvm::FunctionType *FT = llvm::FunctionType::get(ptrType, types, false);

  llvm::Function *F =
      llvm::Function::Create(FT, llvm::Function::ExternalLinkage, name_,
                             codegenContext.context.module);

  // Set names for all arguments.
  unsigned Idx = 0;
  for (auto &arg : F->args())
    arg.setName(args_[Idx++]);

  return F;
}*/

llvm::Value *Function::codegen(CodegenContext &codegenContext) {
  // First, check for an existing function from a previous 'extern' declaration.
  /*llvm::Function *currentFn =
      codegenContext.context.module.getFunction(proto_.getName());

  if (!currentFn)
    currentFn = proto_.codegen(codegenContext);

  if (!currentFn)
    return nullptr;

  if (!currentFn->empty())
    throw std::runtime_error("Function cannot be redefined.");*/
  auto ptrType = codegenContext.type_manager.getPtrType();
  auto envTy = codegenContext.type_manager.getEnvTy();
  auto &builder = codegenContext.context.builder;
  auto size = args_.size();
  std::vector<llvm::Type *> types;
  types.reserve(1 + size);
  types.push_back(envTy);
  for (size_t i = 0; i < size; ++i)
    types.push_back(ptrType);
  llvm::FunctionType *FT = llvm::FunctionType::get(ptrType, types, false);

  llvm::Function *F = llvm::Function::Create(
      FT, llvm::Function::InternalLinkage, getGlobalFnName(std::string(name_)),
      codegenContext.context.module);

  // Set names for all arguments.
  auto &arg = *F->arg_begin();
  arg.setName("_____env");

  unsigned Idx = 0;
  auto it1 = F->arg_begin(); // ugly, FIXME
  it1++;
  for (; it1 != F->arg_end(); it1++)
    it1->setName(args_[Idx++]);

  // Create a new basic block to start insertion into.
  llvm::BasicBlock *BB =
      llvm::BasicBlock::Create(codegenContext.context.context, "entry", F);
  builder.SetInsertPoint(BB);

  codegenContext.lexenv.enterScope();
  auto it = F->arg_begin();
  it++;
  for (; it != F->arg_end(); it++) {
    llvm::AllocaInst *Alloca =
        CreateEntryBlockAlloca(F, codegenContext.type_manager.getPtrType(),
                               std::string(it->getName()));

    codegenContext.context.builder.CreateStore(&*it, Alloca);

    codegenContext.lexenv.addVar(std::string(it->getName()), Alloca);
  }

  if (llvm::Value *RetVal = body_->codegen(codegenContext)) {
    codegenContext.context.builder.CreateRet(RetVal);

    codegenContext.lexenv.exitScope();

    llvm::verifyFunction(*F);

    // Optimizer::getFPM().run(*F, Optimizer::getFAM());
    auto freeVarsAndNames = codegenContext.lexenv.popFreeVars();
    std::vector<llvm::AllocaInst *> freeVars;
    freeVars.reserve((freeVarsAndNames.size()));
    for (auto &[_, inst] : freeVarsAndNames) {
      freeVars.push_back(inst);
    }

    codegenContext.lexenv.addPendingClosure(F, name_, std::move(freeVars),
                                            args_.size());

    return F;
  }

  codegenContext.lexenv.exitScope();
  // Error reading body, remove function.
  F->eraseFromParent();
  return nullptr;
}

llvm::Value *BuiltInOp::codegen(CodegenContext &codegenContext) {
  // Special case '=' because we don't want to emit the LHS as an expression.
  if (name_ == "setq") {

    auto LHSE = static_cast<Variable *>(fst_.get());
    if (!LHSE)
      throw std::runtime_error("destination of '=' must be a variable");
    auto name = LHSE->getName();
    auto [var, is_local] = codegenContext.lexenv.lookUpVar(name);
    auto &builder = codegenContext.context.builder;

    if (!var.get() && !var.getGlob())
      throw std::runtime_error("Unknown variable name");

    // Codegen the RHS.
    llvm::Value *Val = snd_->codegen(codegenContext);
    if (!Val)
      return nullptr;

    if (!is_local && !var.getGlob()) {
      codegenContext.lexenv.addFreeVar(name_, var.get());

      auto idx = codegenContext.lexenv.freeVarsSize() -
                 1; // idx after I add the var, so that later when we emit the
                    // environment it would have this exact idxx for this exact
                    // variable
      auto curEnv = codegenContext.lexenv.getCurrentEnv();

      auto slotPtr = builder.CreateInBoundsGEP(
          codegenContext.type_manager.getEnvTy(), // the Env struct type
          curEnv,                                 // Env*
          {
              builder.getInt32(0),  // index into the pointer itself
              builder.getInt32(1),  // field #1 = slots
              builder.getInt64(idx) // element in the array
          },
          "env.slot.ptr");

      builder.CreateStore(Val, slotPtr);
    } else
      // if not local, instead of a load need GEP + load
      if (auto local = var.get()) {
        builder.CreateStore(Val, local);
      } else {
        builder.CreateStore(Val, var.getGlob());
      }
    return Val;
  }
  return nullptr;
}

llvm::Value *If::codegen(CodegenContext &codegenContext) {
  auto &builder = codegenContext.context.builder;
  auto &context = codegenContext.context.context;

  // 1) Generate the boxed Value* for the condition
  llvm::Value *condBoxed = Cond->codegen(codegenContext);
  if (!condBoxed)
    return nullptr;

  auto rawCond = codegenContext.type_manager.checkAndUnpack(
      codegenContext, condBoxed, Type::Int);

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
      builder.CreatePHI(codegenContext.type_manager.getPtrType(), 2, "iftmp");

  PN->addIncoming(ThenV, ThenBB);
  PN->addIncoming(ElseV, ElseBB);
  return PN;
}

llvm::Value *Goto::codegen(CodegenContext &codegenContext) {
  llvm::Function *currentFn =
      codegenContext.context.builder.GetInsertBlock()->getParent();
  auto &builder = codegenContext.context.builder;

  auto lastVal = CreateEntryBlockAlloca(
      currentFn, codegenContext.type_manager.getPtrType(), "tagbody.ret");

  codegenContext.lexenv.tagEnvs().emplace_back();

  for (auto &item : body_) {
    if (auto tag =
            std::get_if<std::string>(&item)) { // check for the repeating tags
      codegenContext.lexenv.lastTagEnv()[*tag] = llvm::BasicBlock::Create(
          codegenContext.context.context, *tag, currentFn);
    }
  }

  llvm::BasicBlock *afterBB = llvm::BasicBlock::Create(
      codegenContext.context.context,
      "tagbody.exit" + std::to_string(codegenContext.lexenv.tagEnvs().size()),
      currentFn);

  llvm::BasicBlock *curBB = builder.GetInsertBlock();

  for (auto &item : body_) {
    if (auto tag = std::get_if<std::string>(&item)) {
      if (!curBB->getTerminator())
        builder.CreateBr(codegenContext.lexenv.lastTagEnv()[*tag]);
      builder.SetInsertPoint(codegenContext.lexenv.lastTagEnv()[*tag]);
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
  llvm::Value *last = builder.CreateLoad(
      codegenContext.type_manager.getPtrType(), lastVal, "tagbody.last");
  codegenContext.lexenv.tagEnvs().pop_back();

  return last;
}

llvm::Value *Go::codegen(CodegenContext &codegenContext) {
  llvm::BasicBlock *dest = nullptr;
  for (auto env = codegenContext.lexenv.tagEnvs().rbegin();
       env != codegenContext.lexenv.tagEnvs().rend(); ++env) {
    auto it = env->find(tag_);
    if (it != env->end()) {
      dest = it->second;
      break;
    }
  }

  if (!dest)
    throw std::runtime_error("Undefined tag in go: " + tag_);

  codegenContext.context.builder.CreateBr(dest);

  return nullptr;
}

llvm::Value *Let::codegen(CodegenContext &codegenContext) {
  llvm::Function *currentFn =
      codegenContext.context.builder.GetInsertBlock()->getParent();

  // Emit the initializer before adding the variable to scope, this prevents
  // the initializer from referencing the variable itself.
  llvm::Value *initVal;

  initVal = init_->codegen(codegenContext);
  if (!initVal)
    return nullptr;

  llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(
      currentFn, codegenContext.type_manager.getPtrType(), name_);
  codegenContext.context.builder.CreateStore(initVal, Alloca);

  // Remember the old variable binding so that we can restore the binding when
  // we unrecurse.
  codegenContext.lexenv.enterScope();

  // Remember this binding.
  codegenContext.lexenv.addVar(name_, Alloca);

  // Codegen the body, now that all vars are in scope.
  llvm::Value *BodyVal = body_->codegen(codegenContext);
  if (!BodyVal)
    return nullptr;

  // Pop our variable from scope.
  codegenContext.lexenv.exitScope();

  // Return the body computation.
  return BodyVal;
}