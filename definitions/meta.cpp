#include "meta.h"
#include "prepare.h"
#include "util.h"

CodegenContext::CodegenContext()
    : context(), memory_manager(context), type_manager(context), lexenv(*this) {
}

CodegenContext::IRGenContext::IRGenContext()
    : context(llvm::LLVMContext()), builder(llvm::IRBuilder<>(context)),
      module(llvm::Module("my lisp", context)) {}

CodegenContext::SymbolTable::SymbolTable(CodegenContext &codegenContext)
    : trapFn_(llvm::Intrinsic::getDeclaration(&codegenContext.context.module,
                                              llvm::Intrinsic::trap)),
      parent_(&codegenContext) {

  builtInFns_["panic"] = emitPanic(codegenContext);
  builtInFns_["boxInt"] = emitBoxInt(codegenContext);
  builtInFns_["unboxInt"] = emitUnBoxInt(codegenContext);
  builtInFns_[getBuiltInName("cons")] = emitCons(codegenContext);
  builtInFns_["boxCons"] = emitBoxCons(codegenContext);
  builtInFns_["unboxCons"] = emitUnBoxCons(codegenContext);
  builtInFns_[getBuiltInName("car")] = emitCar(codegenContext);
  builtInFns_[getBuiltInName("cdr")] = emitCdr(codegenContext);
  builtInFns_["unboxFn"] = emitUnBoxFn(codegenContext);

  builtInFns_["+"] = emitBuiltIn<2>(
      codegenContext, getBuiltInName("+"),
      [](llvm::IRBuilder<> &builder, llvm::ArrayRef<llvm::Value *> a) {
        return builder.CreateAdd(a[0], a[1], "addtmp");
      },
      getBuiltInFn("unboxInt"), getBuiltInFn("boxInt"));
  builtInFns_["-"] = emitBuiltIn<2>(
      codegenContext, getBuiltInName("-"),
      [](llvm::IRBuilder<> &builder, llvm::ArrayRef<llvm::Value *> a) {
        return builder.CreateSub(a[0], a[1], "subtmp");
      },
      getBuiltInFn("unboxInt"), getBuiltInFn("boxInt"));
  builtInFns_["*"] = emitBuiltIn<2>(
      codegenContext, getBuiltInName("*"),
      [](llvm::IRBuilder<> &builder, llvm::ArrayRef<llvm::Value *> a) {
        return builder.CreateMul(a[0], a[1], "multmp");
      },
      getBuiltInFn("unboxInt"), getBuiltInFn("boxInt"));
  builtInFns_["<"] = emitBuiltIn<2>(
      codegenContext, getBuiltInName("<"),
      [](llvm::IRBuilder<> &builder, llvm::ArrayRef<llvm::Value *> a) {
        auto boolRes = builder.CreateICmpSLT(a[0], a[1], "cmptmp");
        return builder.CreateZExt(boolRes, builder.getInt64Ty(), "booltoint");
      },
      getBuiltInFn("unboxInt"), getBuiltInFn("boxInt"));

  builtInFns_["cons"] = emitBuiltIn<2>( // FIXME should I even cache normal
                                        // (even if predefined) functions?
      codegenContext, "cons",
      [this](llvm::IRBuilder<> &builder, llvm::ArrayRef<llvm::Value *> a) {
        return builder.CreateCall(getBuiltInFn(getBuiltInName("cons")),
                                  {a[0], a[1]}, "cons.ret");
      },
      nullptr, getBuiltInFn("boxCons"));
  builtInFns_["car"] = emitBuiltIn<1>(
      codegenContext, "car",
      [this](llvm::IRBuilder<> &builder, llvm::ArrayRef<llvm::Value *> a) {
        auto call = builder.CreateCall(getBuiltInFn(getBuiltInName("car")),
                                       {a[0]}, "car.ret");
        call->setOnlyReadsMemory();
        call->setDoesNotThrow();
        return call;
      },
      getBuiltInFn("unboxCons"), nullptr);
  builtInFns_["cdr"] = emitBuiltIn<1>(
      codegenContext, "cdr",
      [this](llvm::IRBuilder<> &builder, llvm::ArrayRef<llvm::Value *> a) {
        auto call = builder.CreateCall(getBuiltInFn(getBuiltInName("cdr")),
                                       {a[0]}, "cdr.ret");
        call->setOnlyReadsMemory();
        call->setDoesNotThrow();
        return call;
      },
      getBuiltInFn("unboxCons"), nullptr);

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

void CodegenContext::SymbolTable::enterScope() { named_values_.emplace_back(); }

void CodegenContext::SymbolTable::exitScope() { named_values_.pop_back(); }

void CodegenContext::SymbolTable::addVar(const std::string &name,
                                         llvm::AllocaInst *inst) {
  named_values_.back().emplace(name, inst);
}

void CodegenContext::SymbolTable::addVar(const std::string &name,
                                         llvm::GlobalVariable *inst) {
  globals_.emplace(name, inst);
}

std::pair<VarInst, bool>
CodegenContext::SymbolTable::lookUpVar(const std::string &name) {
  llvm::AllocaInst *ret = nullptr;
  auto local = false;
  for (auto env = named_values_.rbegin(); env != named_values_.rend(); ++env) {
    auto it = env->find(name);
    if (it != env->end()) {
      ret = it->second;
      local = env == named_values_.rbegin();
      break;
    }
  }
  if (!ret)
    if (auto it = globals_.find(name); it != globals_.end())
      return {it->second, false};

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
          codegenContext.context.module,
          codegenContext.type_manager.getPtrType(),
          /*isConstant=*/false, llvm::GlobalValue::ExternalLinkage,
          llvm::Constant::getNullValue(
              codegenContext.type_manager.getPtrType()),
          fnName_)) {}

void CodegenContext::SymbolTable::addPendingClosure(
    llvm::Function *fnPtr, const std::string &fnName,
    std::vector<llvm::AllocaInst *> &&freeVars, size_t arity) {
  pendingClosures_.emplace_back(*parent_, fnPtr, fnName, std::move(freeVars),
                                arity);
  addVar(fnName, pendingClosures_.back().fnGlobal_);
}

void CodegenContext::SymbolTable::emitPendingClosure(
    llvm::GlobalVariable *global, size_t idx) {
  auto &codegenContext = *parent_;
  auto thisClosure = pendingClosures_[idx];

  auto &builder = codegenContext.context.builder;
  auto &module = codegenContext.context.module;
  auto &dataL = module.getDataLayout();
  auto freeVars = thisClosure.freeVars_;
  auto freeVarsSize = freeVars.size();
  auto F = thisClosure.fnPtr_;
  auto argSize = thisClosure.arity_;

  auto closureTy = codegenContext.type_manager.getClosureTy();

  auto envTy = codegenContext.type_manager.getEnvTy();

  auto ptr = codegenContext.type_manager.getPtrType();

  auto valueTy = codegenContext.type_manager.getValueTy();

  auto fnTypeSize =
      dataL.getTypeAllocSize(codegenContext.type_manager.getValueTy());

  auto closureTypeSize = dataL.getTypeAllocSize(closureTy);

  auto closureBoxed =
      builder.CreateCall(codegenContext.memory_manager.getArenaAllocator(),
                         {builder.getInt64(closureTypeSize)}, "closureVal");
  // no bit cast

  auto headerSize =
      dataL.getTypeAllocSize(codegenContext.type_manager.getEnvTy());
  uint64_t slotsSize =
      freeVarsSize *
      dataL.getTypeAllocSize(codegenContext.type_manager.getPtrType());

  llvm::Value *env =
      builder.CreateCall(codegenContext.memory_manager.getArenaAllocator(),
                         {builder.getInt64(headerSize)});

  llvm::Value *freeVarsArray =
      builder.CreateCall(codegenContext.memory_manager.getArenaAllocator(),
                         {builder.getInt64(slotsSize)});

  auto *lenGEP = builder.CreateStructGEP(envTy, env, 0);
  builder.CreateStore(builder.getInt64(freeVarsSize), lenGEP);
  for (size_t i = 0; i < freeVarsSize; ++i) {
    auto *slotPtr =
        builder.CreateInBoundsGEP(ptr, freeVarsArray, builder.getInt64(i));
    builder.CreateStore(freeVars[i], slotPtr);
  }
  auto *slotsGEP = builder.CreateStructGEP(envTy, env, 1);
  builder.CreateStore(freeVarsArray, slotsGEP);

  auto envGEP = builder.CreateStructGEP(closureTy, closureBoxed, 0, "env.slot");
  builder.CreateStore(env, envGEP);

  auto fnGEP = builder.CreateStructGEP(closureTy, closureBoxed, 1, "fn.slot");
  builder.CreateStore(F, fnGEP);

  auto argSizeGEP =
      builder.CreateStructGEP(closureTy, closureBoxed, 2, "arg.size.slot");
  builder.CreateStore(builder.getInt64(argSize), argSizeGEP);

  auto boxed =
      builder.CreateCall(codegenContext.memory_manager.getArenaAllocator(),
                         {builder.getInt64(fnTypeSize)}, "boxedFn");

  auto fnDescGV = codegenContext.type_manager.getType("Fn");
  auto typeGEP = builder.CreateStructGEP(valueTy, boxed, 0, "type.ptr");
  builder.CreateStore(fnDescGV, typeGEP);

  auto payloadGEP = builder.CreateStructGEP(valueTy, boxed, 1, "payload.ptr");
  builder.CreateStore(closureBoxed, payloadGEP);

  builder.CreateStore(boxed, global);
}

void CodegenContext::SymbolTable::initGlobalCtors() {
  auto &codegenContext = *parent_;
  auto &builder = codegenContext.context.builder;
  auto &module = codegenContext.context.module;
  auto ptr = codegenContext.type_manager.getPtrType();
  auto i32Ty = llvm::Type::getInt32Ty(codegenContext.context.context);
  auto globalSize = pendingClosures_.size();
  auto elemTy = llvm::StructType::get(i32Ty, ptr, ptr);
  auto elemArrayTy = llvm::ArrayType::get(elemTy, globalSize);
  auto initCtors = std::vector<llvm::Constant *>();
  for (size_t i = 0; i < pendingClosures_.size(); i++) {
    auto &pending = pendingClosures_[i];
    auto globalFn = pending.fnGlobal_;
    llvm::FunctionType *FT = llvm::FunctionType::get(
        llvm::Type::getVoidTy(codegenContext.context.context), {ptr},
        /*vararg=*/false);
    llvm::Function *F = llvm::Function::Create(
        FT, llvm::Function::InternalLinkage, "init." + pending.fnName_,
        codegenContext.context.module);

    llvm::BasicBlock *BB =
        llvm::BasicBlock::Create(codegenContext.context.context, "entry", F);

    builder.SetInsertPoint(BB);
    emitPendingClosure(globalFn, i);
    builder.CreateRet(nullptr);

    llvm::Constant *ctorElem =
        llvm::ConstantStruct::get(elemTy, {builder.getInt32(i), F, globalFn});
    initCtors.push_back(ctorElem);
  }

  auto ctorArr = llvm::ConstantArray::get(elemArrayTy, initCtors);
  new llvm::GlobalVariable(module, elemArrayTy, false,
                           llvm::GlobalValue::AppendingLinkage, ctorArr,
                           "llvm.global_ctors");
}

llvm::Value *createClosurecall(CodegenContext &codegenContext,
                               llvm::Value *inst, std::vector<ObjPtr> &args) {
  // if (CalleeF->arg_size() != args_.size())
  //   throw std::runtime_error("Incorrect # arguments passed");

  std::vector<llvm::Value *> ArgsV;
  for (auto &Arg : args) {
    ArgsV.push_back(Arg->codegen(codegenContext));
    if (!ArgsV.back())
      return nullptr;
  }
  auto &builder = codegenContext.context.builder;

  auto closureTy = codegenContext.type_manager.getClosureTy();
  auto ptr = codegenContext.type_manager.getPtrType();

  auto *closurePtr =
      builder.CreateBitCast(inst, closureTy->getPointerTo(), "closurePtrCast");

  auto envGEP = builder.CreateStructGEP(closureTy, closurePtr, 0, "envGEP");
  auto envPtr = builder.CreateLoad(ptr, envGEP, "env");

  auto fnGEP = builder.CreateStructGEP(closureTy, closurePtr, 1, "fnGEP");
  auto fnPtr = builder.CreateLoad(ptr, fnGEP, "fn");

  // let's skip the arg check for now
  std::vector<llvm::Type *> types;
  types.reserve(1 + ArgsV.size());
  types.push_back(ptr);
  for (size_t i = 0; i < ArgsV.size(); ++i)
    types.push_back(ptr);
  std::vector<llvm::Value *> envArgs{envPtr};
  envArgs.insert(envArgs.end(), ArgsV.begin(), ArgsV.end());
  llvm::FunctionType *FT = llvm::FunctionType::get(ptr, types, false);
  auto call = builder.CreateCall(FT, fnPtr, envArgs, "closure.res");

  return call;
}
