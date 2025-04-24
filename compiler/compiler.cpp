#include "meta.h"
#include "objects.h"
#include "types.h"

/// CreateEntryBlockAlloca - Create an alloca instruction in the entry block
/// of the function.  This is used for mutable variables etc.
static llvm::AllocaInst *CreateEntryBlockAlloca(CodegenContext &codegenContext,
                                                llvm::Function *TheFunction,
                                                llvm::Type *type,
                                                const std::string &VarName) {
  llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                         TheFunction->getEntryBlock().begin());
  return TmpB.CreateAlloca(type, nullptr, VarName);
}

llvm::Value *Number::codegen(CodegenContext &codegenContext) {
  llvm::StructType *ValueTy = codegenContext.getValueTy();
  llvm::Function *TheFunction =
      codegenContext.builder().GetInsertBlock()->getParent();
  llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                         TheFunction->getEntryBlock().begin());
  llvm::AllocaInst *boxed = CreateEntryBlockAlloca(
      codegenContext, TheFunction, ValueTy, "num" + std::to_string(value_));

  auto *intDescGV = codegenContext.module().getNamedGlobal("type.Int");
  auto *typeGEP = TmpB.CreateStructGEP(ValueTy, boxed, 0, "type.ptr");
  TmpB.CreateStore(intDescGV, typeGEP);

  //  store the raw i64 payload into the 8-byte union
  auto *payloadGEP = TmpB.CreateStructGEP(ValueTy, boxed, 1, "payload.ptr");
  auto *i64Ptr = TmpB.CreateBitCast(
      payloadGEP, llvm::PointerType::get(TmpB.getInt64Ty(), 0),
      "payload.i64.ptr");
  TmpB.CreateStore(TmpB.getInt64(value_), i64Ptr);

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

  codegenContext.named_values().clear();
  for (auto &Arg : TheFunction->args()) {
    llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(
        codegenContext, TheFunction, codegenContext.getPtrType(),
        std::string(Arg.getName()));

    codegenContext.builder().CreateStore(&Arg, Alloca);

    // 3) register it in the symbol table
    codegenContext.named_values()[std::string(Arg.getName())] = Alloca;
  }

  if (llvm::Value *RetVal = body_->codegen(codegenContext)) {
    codegenContext.builder().CreateRet(RetVal);

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

  auto &builder = codegenContext.builder();
  auto valueTy = codegenContext.getValueTy();
  auto *fn = builder.GetInsertBlock()->getParent();

  llvm::Value *fst = fst_->codegen(codegenContext);
  llvm::Value *snd = snd_->codegen(codegenContext);
  if (!fst || !snd) // a hack
    return nullptr;

  llvm::Value *fstPayloadGEP =
      builder.CreateStructGEP(valueTy, fst, 1, "fst.payload.ptr");
  // reinterpret [8 x i8]* as i64*
  llvm::Value *fstI64Ptr = builder.CreateBitCast(
      fstPayloadGEP, builder.getInt64Ty()->getPointerTo(), "fst.i64.ptr");
  // load the raw i64
  llvm::Value *fstI64 =
      builder.CreateLoad(builder.getInt64Ty(), fstI64Ptr, "fst.unboxed");

  llvm::Value *sndPayloadGEP =
      builder.CreateStructGEP(valueTy, snd, 1, "snd.payload.ptr");
  // reinterpret [8 x i8]* as i64*
  llvm::Value *sndI64Ptr = builder.CreateBitCast(
      sndPayloadGEP, builder.getInt64Ty()->getPointerTo(), "snd.i64.ptr");
  // load the raw i64
  llvm::Value *sndI64 =
      builder.CreateLoad(builder.getInt64Ty(), sndI64Ptr, "snd.unboxed");

  llvm::Value *res = nullptr;
  if (name_ == "+") {
    res = builder.CreateAdd(fstI64, sndI64, "addtmp");
  } else if (name_ == "-") {
    res = builder.CreateSub(fstI64, sndI64, "subtmp");
  } else if (name_ == "*") {
    res = builder.CreateMul(fstI64, sndI64, "multmp");
  } else if (name_ == "<") {
    auto boolRes = builder.CreateICmpSLT(fstI64, sndI64, "cmptmp");
    res = builder.CreateZExt(boolRes, builder.getInt64Ty(), "booltoint");
  } else {
    throw std::runtime_error("Non existent operator");
  }

  llvm::AllocaInst *boxed =
      CreateEntryBlockAlloca(codegenContext, fn, valueTy, "op.result");

  // 4b) store the TypeDesc* into field 0
  llvm::GlobalVariable *intDescGV =
      codegenContext.module().getNamedGlobal("type.Int");
  llvm::Value *typeGEP = builder.CreateStructGEP(valueTy, boxed, 0, "type.ptr");
  builder.CreateStore(intDescGV, typeGEP);

  // 4c) store the i64 sum into field 1
  llvm::Value *payloadGEP =
      builder.CreateStructGEP(valueTy, boxed, 1, "payload.ptr");
  llvm::Value *i64Ptr = builder.CreateBitCast(
      payloadGEP, builder.getInt64Ty()->getPointerTo(), "payload.i64.ptr");
  builder.CreateStore(res, i64Ptr);

  return boxed;
}

llvm::Value *If::codegen(CodegenContext &codegenContext) {
  auto &builder = codegenContext.builder();
  auto &context = codegenContext.context();

  // 1) Generate the boxed Value* for the condition
  llvm::Value *condBoxed = Cond->codegen(codegenContext);
  if (!condBoxed)
    return nullptr;

  // 2) Unbox the payload (i64) out of field #1
  llvm::Value *payloadGEP = builder.CreateStructGEP(
      codegenContext.getValueTy(), condBoxed, 1, "cond.payload.ptr");
  llvm::Value *i64Ptr = builder.CreateBitCast(
      payloadGEP, builder.getInt64Ty()->getPointerTo(), "cond.i64.ptr");
  llvm::Value *rawCond =
      builder.CreateLoad(builder.getInt64Ty(), i64Ptr, "cond.unboxed");

  // 3) Compare i64 != 0 → i1
  llvm::Value *condI1 =
      builder.CreateICmpNE(rawCond, builder.getInt64(0), "ifcond");

  llvm::Function *ThisFunction = builder.GetInsertBlock()->getParent();
  llvm::BasicBlock *ThenBB =
      llvm::BasicBlock::Create(context, "then", ThisFunction);
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
  ThisFunction->insert(ThisFunction->end(), ElseBB);
  builder.SetInsertPoint(ElseBB);

  llvm::Value *ElseV = Else->codegen(codegenContext);
  if (!ElseV)
    return nullptr;

  builder.CreateBr(MergeBB);
  // codegen of 'Else' can change the current block, update ElseBB for the PHI.
  ElseBB = builder.GetInsertBlock();

  ThisFunction->insert(ThisFunction->end(), MergeBB);
  builder.SetInsertPoint(MergeBB);
  llvm::PHINode *PN =
      builder.CreatePHI(codegenContext.getPtrType(), 2, "iftmp");

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
      codegenContext.context(),
      "tagbody.exit" + std::to_string(codegenContext.tagEnvs().size()),
      TheFunction);

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
  auto &builder = codegenContext.builder();
  auto valueTy = codegenContext.getValueTy();
  auto *fn = builder.GetInsertBlock()->getParent();

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

  llvm::AllocaInst *boxed =
      CreateEntryBlockAlloca(codegenContext, fn, valueTy, "op.result");

  llvm::GlobalVariable *intDescGV =
      codegenContext.module().getNamedGlobal("type.Int");
  llvm::Value *typeGEP = builder.CreateStructGEP(valueTy, boxed, 0, "type.ptr");
  builder.CreateStore(intDescGV, typeGEP);

  llvm::Value *payloadGEP =
      builder.CreateStructGEP(valueTy, boxed, 1, "payload.ptr");
  llvm::Value *i64Ptr = builder.CreateBitCast(
      payloadGEP, builder.getInt64Ty()->getPointerTo(), "payload.i64.ptr");
  builder.CreateStore(llvm::ConstantInt::get(
                          llvm::Type::getInt64Ty(codegenContext.context()), 0),
                      i64Ptr);

  builder.CreateBr(dest);

  return boxed;
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