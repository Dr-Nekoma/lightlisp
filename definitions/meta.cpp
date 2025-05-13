#include "meta.h"
#include "prepare.h"
#include "util.h"

CodegenContext::CodegenContext()
    : context(),
      debug(new llvm::GlobalVariable(
          this->context.module, llvm::Type::getInt64Ty(this->context.context),
          /*isConstant=*/false, llvm::GlobalValue::InternalLinkage,
          llvm::ConstantInt::get(llvm::Type::getInt64Ty(this->context.context),
                                 157),
          "Debug")),
      debug2(new llvm::GlobalVariable(
          this->context.module, llvm::Type::getInt64Ty(this->context.context),
          /*isConstant=*/false, llvm::GlobalValue::InternalLinkage,
          llvm::ConstantInt::get(llvm::Type::getInt64Ty(this->context.context),
                                 157),
          "Debug2")),
      type_manager(*this), memory_manager(*this), lexenv(*this) {}

CodegenContext::IRGenContext::IRGenContext()
    : context(llvm::LLVMContext()), builder(llvm::IRBuilder<>(context)),
      module(llvm::Module("my lisp", context)) {}

CodegenContext::SymbolTable::SymbolTable(CodegenContext &codegenContext)
    : trapFn_(llvm::Intrinsic::getDeclaration(&codegenContext.context.module,
                                              llvm::Intrinsic::trap)),
      parent_(&codegenContext) {

  builtInFns_["panic"] = emitPanic(codegenContext);
  builtInFns_[getBuiltInName("cons")] = emitCons(codegenContext);
  builtInFns_[getBuiltInName("car")] = emitCar(codegenContext);
  builtInFns_[getBuiltInName("cdr")] = emitCdr(codegenContext);

  builtInFns_["+"] = emitBuiltIn<2>(
      codegenContext, getBuiltInName("+"),
      [](llvm::IRBuilder<> &builder, llvm::ArrayRef<llvm::Value *> a) {
        return builder.CreateAdd(a[0], a[1], "addtmp");
      },
      {Type::Int, Type::Int}, Type::Int);
  builtInFns_["-"] = emitBuiltIn<2>(
      codegenContext, getBuiltInName("-"),
      [](llvm::IRBuilder<> &builder, llvm::ArrayRef<llvm::Value *> a) {
        return builder.CreateSub(a[0], a[1], "subtmp");
      },
      {Type::Int, Type::Int}, Type::Int);
  builtInFns_["*"] = emitBuiltIn<2>(
      codegenContext, getBuiltInName("*"),
      [](llvm::IRBuilder<> &builder, llvm::ArrayRef<llvm::Value *> a) {
        return builder.CreateMul(a[0], a[1], "multmp");
      },
      {Type::Int, Type::Int}, Type::Int);
  builtInFns_["<"] = emitBuiltIn<2>(
      codegenContext, getBuiltInName("<"),
      [](llvm::IRBuilder<> &builder, llvm::ArrayRef<llvm::Value *> a) {
        auto boolRes = builder.CreateICmpSLT(a[0], a[1], "cmptmp");
        return builder.CreateZExt(boolRes, builder.getInt64Ty(), "booltoint");
      },
      {Type::Int, Type::Int}, Type::Int);

  builtInFns_["cons"] = emitBuiltIn<2>( // FIXME should I even cache normal
                                        // (even if predefined) functions?
      codegenContext, "cons",
      [this](llvm::IRBuilder<> &builder, llvm::ArrayRef<llvm::Value *> a) {
        return builder.CreateCall(getBuiltInFn(getBuiltInName("cons")),
                                  {a[0], a[1]}, "cons.ret");
      },
      {std::nullopt}, Type::Cons);
  builtInFns_["car"] = emitBuiltIn<1>(
      codegenContext, "car",
      [this](llvm::IRBuilder<> &builder, llvm::ArrayRef<llvm::Value *> a) {
        auto call = builder.CreateCall(getBuiltInFn(getBuiltInName("car")),
                                       {a[0]}, "car.ret");
        call->setOnlyReadsMemory();
        call->setDoesNotThrow();
        return call;
      },
      {Type::Cons}, std::nullopt);
  builtInFns_["cdr"] = emitBuiltIn<1>(
      codegenContext, "cdr",
      [this](llvm::IRBuilder<> &builder, llvm::ArrayRef<llvm::Value *> a) {
        auto call = builder.CreateCall(getBuiltInFn(getBuiltInName("cdr")),
                                       {a[0]}, "cdr.ret");
        call->setOnlyReadsMemory();
        call->setDoesNotThrow();
        return call;
      },
      {Type::Cons}, std::nullopt);

  // FIXME nil init looks bad and not const
  /*auto *zeroPayload = llvm::ConstantAggregateZero::get(
      llvm::ArrayType::get(builder().getInt8Ty(), 8));
  auto *init = llvm::ConstantStruct::get(
      getValueTy(), {module().getNamedGlobal("type.Cons"), zeroPayload});
  named_values_.front().emplace(
      "nil", new llvm::GlobalVariable(module(), getValueTy(), true,
                                      llvm::GlobalValue::InternalLinkage, init,
                                      "nil"));*/
}

void CodegenContext::SymbolTable::enterScope(bool isFnScope) {
  named_values_.env_stack.emplace_back();
  if (isFnScope)
    named_values_.fn_idxes.emplace_back(named_values_.env_stack.size() - 1);
}

void CodegenContext::SymbolTable::exitScope(bool isFnScope) {
  named_values_.env_stack.pop_back();
  if (isFnScope)
    named_values_.fn_idxes.pop_back();
}

void CodegenContext::SymbolTable::addVar(const std::string &name,
                                         llvm::AllocaInst *inst) {
  named_values_.env_stack.back().emplace(name, inst);
}

void CodegenContext::SymbolTable::addVar(const std::string &name,
                                         llvm::GlobalVariable *inst) {
  globals_.emplace(name, inst);
}

std::pair<VarInst, CodegenContext::SymbolTable::VarStatus>
CodegenContext::SymbolTable::lookUpVar(const std::string &name) {
  llvm::AllocaInst *ret = nullptr;
  auto local = CodegenContext::SymbolTable::VarStatus::NotFound;
  auto idx = named_values_.env_stack.size() - 1;
  for (auto env = named_values_.env_stack.rbegin();
       env != named_values_.env_stack.rend(); ++env, idx--) {
    auto it = env->find(name);
    if (it != env->end()) {
      ret = it->second;
      local = idx >= named_values_.fn_idxes.back()
                  ? CodegenContext::SymbolTable::VarStatus::Local
                  : CodegenContext::SymbolTable::VarStatus::Captured;
      break;
    }
  }
  if (!ret)
    if (auto it = globals_.find(name); it != globals_.end())
      return {it->second, CodegenContext::SymbolTable::VarStatus::Global};

  return {ret, local};
}

std::vector<std::unordered_map<std::string, llvm::BasicBlock *>> &
CodegenContext::SymbolTable::tagEnvs() {
  return tagEnvs_;
}

llvm::Function *
CodegenContext::SymbolTable::getBuiltInFn(const std::string &name) {
  if (auto it = builtInFns_.find(name); it != builtInFns_.end()) {
    return it->second;
  }
  return parent_->context.module.getFunction(name);
}

std::unordered_map<std::string, llvm::BasicBlock *> &
CodegenContext::SymbolTable::lastTagEnv() {
  if (tagEnvs_.empty())
    throw std::runtime_error("Go outside of tagbody");
  return tagEnvs_.back();
}

llvm::Function *CodegenContext::SymbolTable::getTrapFn() { return trapFn_; }

void CodegenContext::SymbolTable::addFreeVar(const std::string &name,
                                             llvm::AllocaInst *var) {
  if (freeVars_.empty()) // no the best fix maybe
    freeVars_.emplace_back();
  auto &last = freeVars_.back();
  for (auto &pair : last) {
    if (pair.first == name) {
      pair.second = var;
      return;
    }
  }
  last.emplace_back(name, var);
}

std::vector<std::pair<std::string, llvm::AllocaInst *>>
CodegenContext::SymbolTable::popFreeVars() {
  if (freeVars_.empty())
    return {};
  auto last = freeVars_.back();
  freeVars_.pop_back();
  return last;
}

size_t CodegenContext::SymbolTable::freeVarsSize() {
  if (freeVars_.empty())
    return 0;
  return freeVars_.back().size();
}

llvm::Value *CodegenContext::SymbolTable::getCurrentEnv() {
  return envStack_.back();
}

CodegenContext::SymbolTable::PendingClosures::PendingClosures(
    CodegenContext &codegenContext, llvm::Function *fnPtr,
    const std::string &fnName, std::vector<llvm::AllocaInst *> &&freeVars,
    size_t arity)
    : fnPtr_(fnPtr), fnName_(fnName), freeVars_(std::move(freeVars)),
      arity_(arity),
      fnGlobal_(new llvm::GlobalVariable(
          codegenContext.context.module, codegenContext.type_manager.ptrType,
          /*isConstant=*/false, llvm::GlobalValue::ExternalLinkage,
          llvm::Constant::getNullValue(codegenContext.type_manager.ptrType),
          fnName_)) {}

void CodegenContext::SymbolTable::addPendingClosure(
    llvm::Function *fnPtr, const std::string &fnName,
    std::vector<llvm::AllocaInst *> &&freeVars, size_t arity) {
  pendingClosures_.emplace_back(*parent_, fnPtr, fnName, std::move(freeVars),
                                arity);
  addVar(fnName, pendingClosures_.back().fnGlobal_);
}

void CodegenContext::SymbolTable::emitPendingClosure(llvm::Value *global,
                                                     size_t idx) {
  auto &codegenContext = *parent_;
  auto thisClosure = pendingClosures_[idx];

  auto &[context, builder, module] = codegenContext.context;
  auto &dataL = module.getDataLayout();
  auto freeVars = thisClosure.freeVars_;
  auto freeVarsSize = freeVars.size();
  auto F = thisClosure.fnPtr_;
  auto argSize = thisClosure.arity_;

  auto closureType = codegenContext.type_manager.closureType;

  auto envType = codegenContext.type_manager.envType;

  auto ptrType = codegenContext.type_manager.ptrType;
  auto closureTypeSize = dataL.getTypeAllocSize(closureType);

  auto closureBoxed =
      builder.CreateCall(codegenContext.memory_manager.getArenaAllocator(),
                         {builder.getInt64(closureTypeSize)}, "closureVal");
  // no bit cast
  auto envGEP =
      builder.CreateStructGEP(closureType, closureBoxed, 0, "env.slot");

  uint64_t slotsSize = freeVarsSize * dataL.getTypeAllocSize(ptrType);

  llvm::Value *freeVarsArray =
      builder.CreateCall(codegenContext.memory_manager.getArenaAllocator(),
                         {builder.getInt64(slotsSize)});

  auto *lenGEP = builder.CreateStructGEP(envType, envGEP, 0);
  builder.CreateStore(builder.getInt64(freeVarsSize), lenGEP);
  for (size_t i = 0; i < freeVarsSize; ++i) {
    auto *slotPtr =
        builder.CreateInBoundsGEP(ptrType, freeVarsArray, builder.getInt64(i));
    builder.CreateStore(freeVars[i], slotPtr);
  }
  auto *slotsGEP = builder.CreateStructGEP(envType, envGEP, 1);
  builder.CreateStore(freeVarsArray, slotsGEP);

  auto fnGEP = builder.CreateStructGEP(closureType, closureBoxed, 1, "fn.slot");
  builder.CreateStore(F, fnGEP);

  auto argSizeGEP =
      builder.CreateStructGEP(closureType, closureBoxed, 2, "arg.size.slot");
  builder.CreateStore(builder.getInt64(argSize), argSizeGEP);

  auto boxed = codegenContext.type_manager.packVal(closureBoxed, Type::Fn);
  builder.CreateStore(boxed, global);
}

void CodegenContext::SymbolTable::initGlobalCtors() {
  auto &codegenContext = *parent_;
  auto &[context, builder, module] = codegenContext.context;
  auto ptrType = codegenContext.type_manager.ptrType;
  for (size_t i = 0; i < pendingClosures_.size(); i++) {
    auto &pending = pendingClosures_[i];
    auto globalFn = pending.fnGlobal_;
    llvm::FunctionType *FT =
        llvm::FunctionType::get(llvm::Type::getVoidTy(context), {ptrType},
                                /*vararg=*/false);
    llvm::Function *F = llvm::Function::Create(
        FT, llvm::Function::InternalLinkage, "init." + pending.fnName_, module);

    llvm::BasicBlock *BB = llvm::BasicBlock::Create(context, "entry", F);

    builder.SetInsertPoint(BB);
    emitPendingClosure(globalFn, i);
    builder.CreateRet(nullptr);

    parent_->addCtor(i + 5, F);
  }
}

llvm::Value *createClosurecall(CodegenContext &codegenContext,
                               llvm::Value *inst, std::vector<ObjPtr> &args) {
  // if (CalleeF->arg_size() != args_.size())
  //   throw std::runtime_error("Incorrect # arguments passed");

  auto &[context, builder, module] = codegenContext.context;

  auto closureType = codegenContext.type_manager.closureType;
  auto ptrType = codegenContext.type_manager.ptrType;
  auto envType = codegenContext.type_manager.envType;

  auto envGEP = builder.CreateStructGEP(closureType, inst, 0, "envGEP");
  auto envPtr = builder.CreateLoad(envType, envGEP, "env");

  auto fnGEP = builder.CreateStructGEP(closureType, inst, 1, "fnGEP");
  auto fnPtr = builder.CreateLoad(ptrType, fnGEP, "fn");

  std::vector<llvm::Value *> ArgsV;
  for (auto &Arg : args) {
    ArgsV.push_back(Arg->codegen(codegenContext).get());
    if (!ArgsV.back())
      return nullptr;
  }

  // let's skip the arg check for now
  std::vector<llvm::Value *> envArgs{envPtr};
  envArgs.insert(envArgs.end(), ArgsV.begin(), ArgsV.end());
  llvm::FunctionType *FT =
      codegenContext.type_manager.getStdFnType(ArgsV.size());

  auto call = builder.CreateCall(FT, fnPtr, envArgs, "closure.res");

  return call;
}

void CodegenContext::addCtor(size_t priority, llvm::Function *ctor) {
  auto ptrType = type_manager.ptrType;
  auto elemType = llvm::StructType::get(type_manager.i32Type, ptrType, ptrType);
  llvm::Constant *ctorElem = llvm::ConstantStruct::get(
      elemType, {context.builder.getInt32(priority), ctor,
                 llvm::Constant::getNullValue(ptrType)});
  initCtors_.push_back(ctorElem);
}

void CodegenContext::emitCtors() {
  auto ptrType = type_manager.ptrType;
  auto globalSize = initCtors_.size();
  auto elemType = llvm::StructType::get(type_manager.i32Type, ptrType, ptrType);
  auto elemArrayType = llvm::ArrayType::get(elemType, globalSize);

  auto ctorArr = llvm::ConstantArray::get(elemArrayType, initCtors_);
  new llvm::GlobalVariable(context.module, elemArrayType, false,
                           llvm::GlobalValue::AppendingLinkage, ctorArr,
                           "llvm.global_ctors");
}