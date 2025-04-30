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

  builtInFns_["+"] = emitBuiltIn<2>(
      codegenContext, getBuiltInName("+"),
      [](llvm::IRBuilder<> &builder, llvm::ArrayRef<llvm::Value *> a) {
        return builder.CreateAdd(a[0], a[1], "addtmp");
      },
      getFn("unboxInt"), getFn("boxInt"));
  builtInFns_["-"] = emitBuiltIn<2>(
      codegenContext, getBuiltInName("-"),
      [](llvm::IRBuilder<> &builder, llvm::ArrayRef<llvm::Value *> a) {
        return builder.CreateSub(a[0], a[1], "subtmp");
      },
      getFn("unboxInt"), getFn("boxInt"));
  builtInFns_["*"] = emitBuiltIn<2>(
      codegenContext, getBuiltInName("*"),
      [](llvm::IRBuilder<> &builder, llvm::ArrayRef<llvm::Value *> a) {
        return builder.CreateMul(a[0], a[1], "multmp");
      },
      getFn("unboxInt"), getFn("boxInt"));
  builtInFns_["<"] = emitBuiltIn<2>(
      codegenContext, getBuiltInName("<"),
      [](llvm::IRBuilder<> &builder, llvm::ArrayRef<llvm::Value *> a) {
        auto boolRes = builder.CreateICmpSLT(a[0], a[1], "cmptmp");
        return builder.CreateZExt(boolRes, builder.getInt64Ty(), "booltoint");
      },
      getFn("unboxInt"), getFn("boxInt"));

  builtInFns_["cons"] = emitBuiltIn<2>( // FIXME should I even cache normal
                                        // (even if predefined) functions?
      codegenContext, "cons",
      [this](llvm::IRBuilder<> &builder, llvm::ArrayRef<llvm::Value *> a) {
        return builder.CreateCall(getFn(getBuiltInName("cons")), {a[0], a[1]},
                                  "cons.ret");
      },
      nullptr, getFn("boxCons"));
  builtInFns_["car"] = emitBuiltIn<1>(
      codegenContext, "car",
      [this](llvm::IRBuilder<> &builder, llvm::ArrayRef<llvm::Value *> a) {
        auto call =
            builder.CreateCall(getFn(getBuiltInName("car")), {a[0]}, "car.ret");
        call->setOnlyReadsMemory();
        call->setDoesNotThrow();
        return call;
      },
      getFn("unboxCons"), nullptr);
  builtInFns_["cdr"] = emitBuiltIn<1>(
      codegenContext, "cdr",
      [this](llvm::IRBuilder<> &builder, llvm::ArrayRef<llvm::Value *> a) {
        auto call =
            builder.CreateCall(getFn(getBuiltInName("cdr")), {a[0]}, "cdr.ret");
        call->setOnlyReadsMemory();
        call->setDoesNotThrow();
        return call;
      },
      getFn("unboxCons"), nullptr);

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

llvm::AllocaInst *
CodegenContext::SymbolTable::lookUpVar(const std::string &name) {
  llvm::AllocaInst *ret = nullptr;
  for (auto env = named_values_.rbegin(); env != named_values_.rend(); ++env) {
    auto it = env->find(name);
    if (it != env->end()) {
      ret = it->second;
      break;
    }
  }
  return ret;
}

std::vector<std::unordered_map<std::string, llvm::BasicBlock *>> &
CodegenContext::SymbolTable::tagEnvs() {
  return tagEnvs_;
}

llvm::Function *CodegenContext::SymbolTable::getFn(const std::string &name) {
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