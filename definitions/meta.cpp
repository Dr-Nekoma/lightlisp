#include "meta.h"
#include "prepare.h"
#include "util.h"

CodegenContext::CodegenContext()
    : context(), type_manager(*this), memory_manager(*this), lexenv(*this) {}

CodegenContext::IRGenContext::IRGenContext()
    : context(llvm::LLVMContext()), builder(llvm::IRBuilder<>(context)),
      module(llvm::Module("my lisp", context)) {}

CodegenContext::SymbolTable::SymbolTable(CodegenContext &codegenContext)
    : parent_(&codegenContext) {

  llvm::FunctionType *trapFT =
      llvm::FunctionType::get(parent_->context.builder.getVoidTy(),
                              {parent_->context.builder.getInt32Ty(),
                               parent_->context.builder.getInt32Ty()},
                              false);
  trapFn_ = llvm::Function::Create(trapFT, llvm::Function::ExternalLinkage,
                                   "panic_code", parent_->context.module);

  llvm::FunctionType *printFT =
      llvm::FunctionType::get(parent_->context.builder.getVoidTy(),
                              {parent_->context.builder.getInt64Ty()}, false);
  printFn_ = llvm::Function::Create(printFT, llvm::Function::ExternalLinkage,
                                    "print_int", parent_->context.module);

  builtInFns_["printInt"] = printFn_;
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
  auto ptrType = codegenContext.type_manager.ptrType;
  llvm::FunctionType *FT = llvm::FunctionType::get(
      llvm::Type::getVoidTy(parent_->context.context), {ptrType},
      /*vararg=*/false);
  ctorFn_ = llvm::Function::Create(FT, llvm::Function::InternalLinkage,
                                   "init.Closures", parent_->context.module);

  ctorBlock_ = llvm::BasicBlock::Create(parent_->context.context, "ctors.entry",
                                        ctorFn_);
}

void CodegenContext::SymbolTable::enterScope(llvm::Argument *newEnv) {
  named_values_.env_stack.emplace_back();
  if (newEnv) {
    named_values_.fn_idxes.emplace_back(named_values_.env_stack.size() - 1);
    envStack_.emplace_back(newEnv);
  }
}

void CodegenContext::SymbolTable::exitScope(bool isFnScope) {
  named_values_.env_stack.pop_back();
  if (isFnScope) {
    named_values_.fn_idxes.pop_back();
    envStack_.pop_back();
  }
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

llvm::Argument *CodegenContext::SymbolTable::getCurrentEnv() {
  return envStack_.back();
}

llvm::BasicBlock *CodegenContext::SymbolTable::getCtorBlock() {
  return ctorBlock_;
}

llvm::Function *CodegenContext::SymbolTable::getPrintFn() { return printFn_; }

llvm::Function *CodegenContext::SymbolTable::getCtorFn() { return ctorFn_; }

/*
llvm::BasicBlock *CodegenContext::SymbolTable::getCurContextBlock() {
  return isDef_ ? ctorBlock_ : parent_->context.builder.GetInsertBlock();
}

llvm::Function *CodegenContext::SymbolTable::getCurCtorFn() {
  return isDef_ ? ctorFn_
                : parent_->context.builder.GetInsertBlock()->getParent();
?????????
}
*/
void CodegenContext::SymbolTable::setFnWrapperParameters(
    std::vector<llvm::AllocaInst *> &&vars, size_t size) {
  curFreeVars_ = vars;
  curArgSize_ = size;
}

void CodegenContext::SymbolTable::setInsertBlock(llvm::BasicBlock *block,
                                                 bool descend) {
  if (descend)
    insertBlocks_.push_back(block);
  else if (!insertBlocks_.empty()) // A HACK FIXME TODO
    insertBlocks_.back() = block;
  parent_->context.builder.SetInsertPoint(block);
}

llvm::BasicBlock *CodegenContext::SymbolTable::getCurrentBlock() {
  return insertBlocks_.empty() ? ctorBlock_ : insertBlocks_.back();
}

void CodegenContext::SymbolTable::ascend() {
  insertBlocks_.pop_back();
  parent_->context.builder.SetInsertPoint(getCurrentBlock());
}

llvm::Value *
CodegenContext::SymbolTable::constructClosureWrapper(llvm::Function *fnPtr) {
  auto &codegenContext = *parent_;
  auto &[context, builder, module] = codegenContext.context;
  auto ptrType = codegenContext.type_manager.ptrType;
  auto closureType = codegenContext.type_manager.closureType;
  auto &dataL = module.getDataLayout();

  auto freeVars = std::move(curFreeVars_.value());
  auto arity = std::move(curArgSize_.value());
  curFreeVars_.reset();
  curArgSize_.reset();

  // codegenContext.lexenv.setInsertBlock(getCurrentBlock(), false);

  auto freeVarsSize = freeVars.size();

  auto closureTypeSize = dataL.getTypeAllocSize(closureType);

  auto closureBoxed =
      builder.CreateCall(codegenContext.memory_manager.getArenaAllocator(),
                         {builder.getInt64(closureTypeSize)}, "closureVal");

  auto envGEP = codegenContext.type_manager.getClosureEnv(closureBoxed);

  uint64_t slotsSize = freeVarsSize * dataL.getTypeAllocSize(ptrType);

  llvm::Value *freeVarsArray =
      builder.CreateCall(codegenContext.memory_manager.getArenaAllocator(),
                         {builder.getInt64(slotsSize)});

  codegenContext.type_manager.storeEnvSize(builder.getInt64(freeVarsSize),
                                           envGEP);

  for (size_t i = 0; i < freeVarsSize; ++i) {
    auto *slotPtr =
        builder.CreateInBoundsGEP(ptrType, freeVarsArray, builder.getInt64(i));
    auto actualVal = builder.CreateLoad(
        ptrType, freeVars[i], "loaded.noAlloca"); // more debug names for now
    builder.CreateStore(actualVal, slotPtr);
  }
  codegenContext.type_manager.storeEnvStorage(freeVarsArray, envGEP);

  codegenContext.type_manager.storeClosureFn(fnPtr, closureBoxed);

  codegenContext.type_manager.storeClosureSize(builder.getInt32(arity),
                                               closureBoxed);

  auto boxed = codegenContext.type_manager.packVal(closureBoxed, Type::Fn);
  return boxed;
}

llvm::Value *createClosurecall(CodegenContext &codegenContext,
                               llvm::Value *inst, std::vector<ObjPtr> &args) {
  // if (CalleeF->arg_size() != args_.size())
  //   throw std::runtime_error("Incorrect # arguments passed");

  auto &[context, builder, module] = codegenContext.context;

  auto envGEP = codegenContext.type_manager.getClosureEnv(inst);

  auto fnPtr = codegenContext.type_manager.loadClosureFn(inst);

  std::vector<llvm::Value *> ArgsV;
  for (auto &Arg : args) {
    ArgsV.push_back(Arg->codegen(codegenContext).get());
    if (!ArgsV.back())
      return nullptr;
  }

  // let's skip the arg check for now
  std::vector<llvm::Value *> envArgs{envGEP};
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

void CodegenContext::emitClosuresCtor() {
  context.builder.SetInsertPoint(lexenv.getCtorBlock());
  context.builder.CreateRetVoid();

  addCtor(3, lexenv.getCtorFn());

  auto ptrType = type_manager.ptrType;
  auto globalSize = initCtors_.size();
  auto elemType = llvm::StructType::get(type_manager.i32Type, ptrType, ptrType);
  auto elemArrayType = llvm::ArrayType::get(elemType, globalSize);

  auto ctorArr = llvm::ConstantArray::get(elemArrayType, initCtors_);
  new llvm::GlobalVariable(context.module, elemArrayType, false,
                           llvm::GlobalValue::AppendingLinkage, ctorArr,
                           "llvm.global_ctors");
}
