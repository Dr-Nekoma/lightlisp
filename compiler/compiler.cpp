#include "compiler.h"
#include "Optimizer.h"
#include "meta.h"
#include "util.h"

// Number implementations
Number::Number(int64_t value) : value_(value) {}

int64_t Number::getValue() const { return value_; }

// Symbol implementations
Symbol::Symbol(std::string name) : name_(std::move(name)) {}

const std::string &Symbol::getName() const { return name_; }

// Cell implementations
Cell::Cell() : head_(nullptr), tail_(nullptr) {}

Cell::Cell(std::unique_ptr<SyntaxObject> head,
           std::unique_ptr<SyntaxObject> tail)
    : head_(std::move(head)), tail_(std::move(tail)) {}

// Cell::ListIterator implementations
Cell::ListIterator::ListIterator(SyntaxObject *node) : node_(node) {}

Cell::ListIterator::reference Cell::ListIterator::operator*() const {
  auto &ret = std::get<Cell>(*node_);
  return *ret.head_;
}

Cell::ListIterator::pointer Cell::ListIterator::operator->() const {
  return std::addressof(*node_);
}

Cell::ListIterator::pointer Cell::ListIterator::getCell() { return node_; }

bool Cell::ListIterator::operator==(ListIterator const &o) const {
  return node_ == o.node_;
}

bool Cell::ListIterator::operator!=(ListIterator const &o) const {
  return !(*this == o);
}

Cell::ListIterator &Cell::ListIterator::operator++() {
  if (!node_)
    throw std::runtime_error("increment past end");
  auto c = std::get_if<Cell>(node_);
  if (!c)
    throw std::runtime_error("not a proper list");
  node_ = c->get<1>().get(); /* Follow cdr pointer */
  if (node_ && !std::holds_alternative<Cell>(*node_))
    throw std::runtime_error("not a proper list");
  return *this;
}

Cell::ListIterator Cell::ListIterator::operator++(int) {
  auto tmp = *this;
  ++*this;
  return tmp;
}

// Cell::ListView implementations
Cell::ListView::ListView(SyntaxObject *head, SyntaxObject *tail)
    : head_(head), tail_(tail) {}

Cell::ListView::ListView(SyntaxObject *head) : head_(head), tail_(nullptr) {}

Cell::ListIterator Cell::ListView::begin() const { return ListIterator(head_); }

Cell::ListIterator Cell::ListView::end() const { return ListIterator(tail_); }

// Lambda implementations
Lambda<Expanded>::Lambda(std::vector<Symbol> &&args, ExprPtr<Expanded> body)
    : args_(std::move(args)), body_(std::move(body)) {}

Lambda<NotExpanded>::Lambda(std::vector<Symbol> &&args, IR1Expr body)
    : args_(std::move(args)), body_(std::move(body)) {}

ExprPtr<Expanded> Lambda<NotExpanded>::expand() {
  auto expanded_body_ptr = ir2LispTransform(std::move(body_));

  auto expanded_lambda = Lambda<Expanded>(std::vector<Symbol>(args_),
                                          std::move(expanded_body_ptr));
  return std::make_unique<Lambda<Expanded>>(std::move(expanded_lambda));
}

// Call implementations
Call<Expanded>::Call(ExprPtr<Expanded> callee,
                     std::vector<ExprPtr<Expanded>> args)
    : callee_(std::move(callee)), args_(std::move(args)) {}

Call<NotExpanded>::Call(IR1Expr callee, std::vector<IR1Expr> args)
    : callee_(std::move(callee)), args_(std::move(args)) {}

ExprPtr<Expanded> Call<NotExpanded>::expand() {
  ExprPtr<Expanded> expanded_callee = ir2LispTransform(std::move(callee_));

  std::vector<ExprPtr<Expanded>> expanded_args;
  expanded_args.reserve(args_.size());
  for (auto &arg : args_)
    expanded_args.emplace_back(ir2LispTransform(std::move(arg)));

  return std::make_unique<Call<Expanded>>(
      Call<Expanded>(std::move(expanded_callee), std::move(expanded_args)));
}

// Def implementations
Def<Expanded>::Def(Symbol var, ExprPtr<Expanded> init)
    : var_(std::move(var)), init_(std::move(init)) {}

Def<NotExpanded>::Def(Symbol var, IR1Expr init)
    : var_(std::move(var)), init_(std::move(init)) {}

ExprPtr<Expanded> Def<NotExpanded>::expand() {
  ExprPtr<Expanded> expanded_init = ir2LispTransform(std::move(init_));
  auto expanded_def =
      std::make_unique<Def<Expanded>>(var_, std::move(expanded_init));
  return expanded_def;
}

// Setq implementations
Setq<Expanded>::Setq(Symbol var, ExprPtr<Expanded> newval)
    : var_(std::move(var)), newval_(std::move(newval)) {}

Setq<NotExpanded>::Setq(Symbol var, IR1Expr newval)
    : var_(std::move(var)), newval_(std::move(newval)) {}

ExprPtr<Expanded> Setq<NotExpanded>::expand() {
  ExprPtr<Expanded> expanded_newval = ir2LispTransform(std::move(newval_));
  auto expanded_setq = std::make_unique<Setq<Expanded>>(
      std::move(var_), std::move(expanded_newval));
  return expanded_setq;
}
// If implementations
If<Expanded>::If(ExprPtr<Expanded> cond, ExprPtr<Expanded> thenBranch,
                 ExprPtr<Expanded> elseBranch)
    : cond_(std::move(cond)), then_(std::move(thenBranch)),
      else_(std::move(elseBranch)) {}

If<NotExpanded>::If(IR1Expr cond, IR1Expr thenBranch, IR1Expr elseBranch)
    : cond_(std::move(cond)), then_(std::move(thenBranch)),
      else_(std::move(elseBranch)) {}

ExprPtr<Expanded> If<NotExpanded>::expand() {
  ExprPtr<Expanded> expanded_cond = ir2LispTransform(std::move(cond_));
  ExprPtr<Expanded> expanded_then = ir2LispTransform(std::move(then_));
  ExprPtr<Expanded> expanded_else = ir2LispTransform(std::move(else_));

  auto expanded_if = std::make_unique<If<Expanded>>(std::move(expanded_cond),
                                                    std::move(expanded_then),
                                                    std::move(expanded_else));
  return expanded_if;
}

// Goto implementations
Goto<Expanded>::Goto(
    std::vector<std::variant<ExprPtr<Expanded>, std::string>> &&body)
    : body_(std::move(body)) {}

Goto<NotExpanded>::Goto(std::vector<std::variant<IR1Expr, std::string>> &&body)
    : body_(std::move(body)) {}

ExprPtr<Expanded> Goto<NotExpanded>::expand() {
  std::vector<std::variant<ExprPtr<Expanded>, std::string>> expanded_body;
  expanded_body.reserve(body_.size());

  for (auto &item : body_) {
    if (std::holds_alternative<std::string>(item)) {
      expanded_body.push_back(std::get<std::string>(item));
    } else {
      expanded_body.emplace_back(
          ir2LispTransform(std::move(std::get<IR1Expr>(item))));
    }
  }

  auto expanded_goto =
      std::make_unique<Goto<Expanded>>(std::move(expanded_body));
  return expanded_goto;
}

// Go implementations
Go<Expanded>::Go(std::string &&tag) : tag_(std::move(tag)) {}

Go<NotExpanded>::Go(std::string &&tag) : tag_(std::move(tag)) {}

ExprPtr<Expanded> Go<NotExpanded>::expand() {
  auto expanded_go = std::make_unique<Go<Expanded>>(std::string(tag_));
  return expanded_go;
}

// Let implementations
Let<Expanded>::Let(Symbol var, ExprPtr<Expanded> init, ExprPtr<Expanded> body)
    : var_(std::move(var)), init_(std::move(init)), body_(std::move(body)) {}

Let<NotExpanded>::Let(Symbol var, IR1Expr init, IR1Expr body)
    : var_(std::move(var)), init_(std::move(init)), body_(std::move(body)) {}

ExprPtr<Expanded> Let<NotExpanded>::expand() {
  ExprPtr<Expanded> expanded_init = ir2LispTransform(std::move(init_));
  ExprPtr<Expanded> expanded_body = ir2LispTransform(std::move(body_));

  auto expanded_let = std::make_unique<Let<Expanded>>(
      var_, std::move(expanded_init), std::move(expanded_body));
  return expanded_let;
}

// Codegen impl

llvm::Value *Number::codegen(CodegenContext &codegenContext) {
  auto &[context, builder, module] = codegenContext.context;
  llvm::Value *raw = builder.getInt64(value_);

  auto name = "num" + std::to_string(value_);
  llvm::Value *boxed = codegenContext.type_manager.packVal(raw, Type::Int);

  return boxed;
}

TaggedLLVMVal Symbol::load(CodegenContext &codegenContext) {
  if (auto fn = codegenContext.lexenv.getBuiltInFn(name_); fn) {
    return fn;
  }
  auto [var, status] = codegenContext.lexenv.lookUpVar(name_);
  auto &[context, builder, module] = codegenContext.context;
  switch (status) {
  case CodegenContext::SymbolTable::VarStatus::Local: {
    auto local = var.getLocal();
    return builder.CreateLoad(local->getAllocatedType(), local, name_);
  }
  case CodegenContext::SymbolTable::VarStatus::Captured: {
    codegenContext.lexenv.addFreeVar(name_, var.getLocal());

    auto idx =
        codegenContext.lexenv.freeVarsSize() -
        1; // idx after I add the var, so that later when we emit the
           // environment it would have this exact idxx for this exact variable
    auto curEnv = codegenContext.lexenv.getCurrentEnv();

    auto ptrType = codegenContext.type_manager.ptrType;
    auto slotsPtr = codegenContext.type_manager.loadEnvStorage(curEnv);

    auto pos =
        builder.CreateInBoundsGEP(ptrType, slotsPtr, builder.getInt64(idx));
    auto valCapture = builder.CreateLoad(ptrType, pos, "debug.Capture");

    return valCapture;
  }

  case CodegenContext::SymbolTable::VarStatus::Global: {
    auto global = var.getGlob();
    return builder.CreateLoad(codegenContext.type_manager.ptrType, global,
                              name_);
  }
  case CodegenContext::SymbolTable::VarStatus::GlobalConstant: {
    auto global = var.getGlob();
    return global;
  }
  default:
    throw std::runtime_error("Unknown variable name");
  }
}

llvm::CallInst *Call<Expanded>::emit(CodegenContext &codegenContext) {
  auto &[context, builder, module] = codegenContext.context;
  // Look up the name in the global module table.
  auto calleeF = codegen(codegenContext, callee_);
  if (calleeF.is<llvm::Function *>()) {
    auto fn = calleeF.get<llvm::Function *>();
    // If argument mismatch error.
    if (fn->arg_size() != args_.size())
      throw std::runtime_error("Incorrect # arguments passed");

    std::vector<llvm::Value *> argsV;
    for (auto &arg : args_) {
      argsV.push_back(codegen(codegenContext, arg).get<llvm::Value *>());
      if (!argsV.back())
        return {};
    }

    auto call = builder.CreateCall(fn, argsV, "calltmp");

    return call;
  }
  auto userVar = calleeF.get<llvm::Value *>();
  auto closure = codegenContext.type_manager.checkAndUnpack(userVar, Type::Fn);

  return createClosurecall(codegenContext, closure, args_);
}

llvm::GlobalVariable *Def<Expanded>::load(CodegenContext &codegenContext) {
  auto &[context, builder, module] = codegenContext.context;
  auto &name = var_.getName();

  auto global = new llvm::GlobalVariable(
      codegenContext.context.module, codegenContext.type_manager.ptrType,
      /*isConstant=*/false, llvm::GlobalValue::ExternalLinkage,
      llvm::Constant::getNullValue(codegenContext.type_manager.ptrType), name);

  codegenContext.lexenv.addVar(name, global);

  auto initVal = codegen(codegenContext, init_).get<llvm::Value *>();

  if (!initVal)
    return {};

  builder.CreateStore(initVal, global);

  return global;
}

TaggedLLVMVal Setq<Expanded>::codegen(CodegenContext &codegenContext) {
  auto &[context, builder, module] = codegenContext.context;

  auto name = var_.getName();
  auto [var, status] = codegenContext.lexenv.lookUpVar(name);
  if (status == CodegenContext::SymbolTable::VarStatus::NotFound)
    throw std::runtime_error("Unknown variable name");

  auto val = ::codegen(codegenContext, newval_).get<llvm::Value *>();
  if (!val)
    return {};

  switch (status) {
  case CodegenContext::SymbolTable::VarStatus::Local:
    builder.CreateStore(val, var.getLocal());
    break;
  case CodegenContext::SymbolTable::VarStatus::Captured: {
    codegenContext.lexenv.addFreeVar(name, var.getLocal());

    auto idx = codegenContext.lexenv.freeVarsSize() - 1;
    auto curEnv = codegenContext.lexenv.getCurrentEnv();

    auto ptrType = codegenContext.type_manager.ptrType;
    auto slotsPtr = codegenContext.type_manager.loadEnvStorage(curEnv);

    auto pos =
        builder.CreateInBoundsGEP(ptrType, slotsPtr, builder.getInt64(idx));

    auto val = builder.CreateLoad(ptrType, pos, "stored.val");

    codegenContext.type_manager.copyValInto(val, val);
    break;
  }
  case CodegenContext::SymbolTable::VarStatus::Global:
    builder.CreateStore(val, var.getGlob());
    break;
  case CodegenContext::SymbolTable::VarStatus::GlobalConstant:
    throw std::runtime_error("Cannot setq constants");
  case CodegenContext::SymbolTable::VarStatus::NotFound:
    throw std::runtime_error("Unknown variable name");
  }

  return val;
}

llvm::PHINode *If<Expanded>::codegen(CodegenContext &codegenContext) {
  auto &[context, builder, module] = codegenContext.context;

  // Generate the boxed Value* for the condition
  auto condBoxed = ::codegen(codegenContext, cond_).get<llvm::Value *>();
  if (!condBoxed)
    return {};

  llvm::Value *condI1 = codegenContext.type_manager.emitTrueCheck(condBoxed);

  llvm::Function *currentFn = builder.GetInsertBlock()->getParent();
  llvm::BasicBlock *ThenBB =
      llvm::BasicBlock::Create(context, "then", currentFn);
  llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(context, "else");
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(context, "ifcont");

  builder.CreateCondBr(condI1, ThenBB, ElseBB);

  codegenContext.lexenv.setInsertBlock(ThenBB, false);

  auto thenV = ::codegen(codegenContext, then_).get<llvm::Value *>();
  if (!thenV)
    return {};

  builder.CreateBr(MergeBB);
  // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
  ThenBB = builder.GetInsertBlock();

  // Emit else block.
  currentFn->insert(currentFn->end(), ElseBB);
  codegenContext.lexenv.setInsertBlock(ElseBB, false);

  auto elseV = ::codegen(codegenContext, else_).get<llvm::Value *>();
  if (!elseV)
    return {};

  builder.CreateBr(MergeBB);
  // codegen of 'Else' can change the current block, update ElseBB for the PHI.
  ElseBB = builder.GetInsertBlock();

  currentFn->insert(currentFn->end(), MergeBB);
  codegenContext.lexenv.setInsertBlock(MergeBB, false);
  llvm::PHINode *PN =
      builder.CreatePHI(codegenContext.type_manager.ptrType, 2, "iftmp");

  PN->addIncoming(thenV, ThenBB);
  PN->addIncoming(elseV, ElseBB);
  return PN;
}

TaggedLLVMVal Goto<Expanded>::codegen(CodegenContext &codegenContext) {
  llvm::Function *currentFn =
      codegenContext.context.builder.GetInsertBlock()->getParent();
  auto &[context, builder, module] = codegenContext.context;

  // Check if this is a simple sequential body with no tags
  auto nonTrivialTags = std::any_of(body_.begin(), body_.end(), [](auto &item) {
    return std::holds_alternative<std::string>(item);
  });
  if (!nonTrivialTags) {
    llvm::Value *ret;
    for (auto &item : body_) {
      auto &expr = std::get<ExprPtr<Expanded>>(item);
      ret = ::codegen(codegenContext, expr).get<llvm::Value *>();
    }
    return ret;
  }

  // Storage for preserving the last evaluated expression value
  auto lastVal = CreateEntryBlockAlloca(
      currentFn, codegenContext.type_manager.ptrType, "tagbody.ret");

  codegenContext.lexenv.tagEnvs().emplace_back();

  for (auto &item : body_) {
    if (auto tag =
            std::get_if<std::string>(&item)) { // check for the repeating tags
      codegenContext.lexenv.lastTagEnv()[*tag] =
          llvm::BasicBlock::Create(context, *tag, currentFn);
    }
  }

  llvm::BasicBlock *afterBB = llvm::BasicBlock::Create(
      context,
      "tagbody.exit" + std::to_string(codegenContext.lexenv.tagEnvs().size()),
      currentFn);

  llvm::BasicBlock *curBB = builder.GetInsertBlock();

  for (auto &item : body_) {
    if (auto tag = std::get_if<std::string>(&item)) {
      if (!curBB->getTerminator())
        builder.CreateBr(codegenContext.lexenv.lastTagEnv()[*tag]);
      codegenContext.lexenv.setInsertBlock(
          codegenContext.lexenv.lastTagEnv()[*tag], false);
    } else {
      auto &expr = std::get<ExprPtr<Expanded>>(item);
      auto v = ::codegen(codegenContext, expr).get<llvm::Value *>();
      if (v)
        builder.CreateStore(v, lastVal);
      curBB = builder.GetInsertBlock();
    }
  }

  if (!curBB->getTerminator())
    builder.CreateBr(afterBB);
  codegenContext.lexenv.setInsertBlock(afterBB, false);
  llvm::Value *last = builder.CreateLoad(codegenContext.type_manager.ptrType,
                                         lastVal, "tagbody.last");
  codegenContext.lexenv.tagEnvs().pop_back();

  return last;
}

void Go<Expanded>::codegen(CodegenContext &codegenContext) {
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
}

TaggedLLVMVal Let<Expanded>::codegen(CodegenContext &codegenContext) {
  llvm::Function *currentFn =
      codegenContext.lexenv.getCurrentBlock()->getParent();
  // Emit the initializer before adding the variable to scope, this prevents
  // the initializer from referencing the variable itself.
  auto initVal = ::codegen(codegenContext, init_).get<llvm::Value *>();

  codegenContext.lexenv.createLocalVar(currentFn, initVal, var_.getName(),
                                       true);

  auto bodyVal = ::codegen(codegenContext, body_).get<llvm::Value *>();
  if (!bodyVal)
    return {};

  // Pop our variable from scope.
  codegenContext.lexenv.exitScope(false);

  return bodyVal;
}

llvm::Value *Lambda<Expanded>::codegen(CodegenContext &codegenContext) {
  auto &[context, builder, module] = codegenContext.context;
  auto size = args_.size();
  llvm::FunctionType *FT = codegenContext.type_manager.getStdFnType(size);

  llvm::Function *F = llvm::Function::Create(
      FT, llvm::Function::InternalLinkage, "__anon__closure", module);

  // First argument is always the environment pointer for closures
  auto &arg = *F->arg_begin();
  arg.setName("_____env"); // FIXME -- normal naming for hidden args

  unsigned Idx = 0;
  auto it1 = F->arg_begin(); // FIXME, merge with above
  it1++;
  for (; it1 != F->arg_end(); it1++)
    it1->setName(args_[Idx++].getName());

  auto BB = llvm::BasicBlock::Create(context, "entry", F);

  codegenContext.lexenv.setInsertBlock(BB, true);

  codegenContext.lexenv.enterScope(&*F->arg_begin());
  auto it = F->arg_begin();
  it++;
  for (; it != F->arg_end(); it++)
    codegenContext.lexenv.createLocalVar(F, &*it, it->getName().str());

  auto retVal = ::codegen(codegenContext, body_).get<llvm::Value *>();
  if (!retVal) {
    codegenContext.lexenv.exitScope(true);
    // Error reading body, remove function.
    F->eraseFromParent();
    return {};
  }
  builder.CreateRet(retVal);

  codegenContext.lexenv.exitScope(true);
  codegenContext.lexenv.ascend();

  llvm::verifyFunction(*F);

  // Optimizer::getFPM().run(*F, Optimizer::getFAM());
  auto freeVarsAndNames = codegenContext.lexenv.popFreeVars();

  std::vector<llvm::AllocaInst *> freeVars;
  freeVars.reserve((freeVarsAndNames.size()));
  for (auto &[_, inst] : freeVarsAndNames) {
    freeVars.push_back(inst);
  }

  codegenContext.lexenv.setFnWrapperParameters(std::move(freeVars),
                                               args_.size());
  auto boxed = codegenContext.lexenv.constructClosureWrapper(F);

  return boxed;
}

TaggedLLVMVal codegen(CodegenContext &context, ExprPtr<Expanded> &expr) {
  return std::visit(
      [&context](auto &&expr_variant) -> TaggedLLVMVal {
        using T = std::decay_t<decltype(expr_variant)>;

        if constexpr (std::is_same_v<T, AtomPtr>) {
          return std::visit(
              [&context](auto &&atom) -> TaggedLLVMVal {
                using AtomType = std::decay_t<decltype(atom)>;

                if constexpr (std::is_same_v<AtomType,
                                             std::unique_ptr<Number>>) {
                  return atom->codegen(context);
                } else if constexpr (std::is_same_v<AtomType,
                                                    std::unique_ptr<Symbol>>) {
                  return atom->load(context);
                } else {
                  throw std::runtime_error("Unsupported Atom type encountered");
                }
              },
              expr_variant);
        } else if constexpr (std::is_same_v<T, SpFPtr<Expanded>>) {
          return std::visit(
              [&context](auto &&spf) -> TaggedLLVMVal {
                using SPFType = std::decay_t<decltype(spf)>;

                if constexpr (std::is_same_v<SPFType,
                                             std::unique_ptr<Call<Expanded>>>) {
                  return spf->emit(context);
                } else if constexpr (std::is_same_v<
                                         SPFType,
                                         std::unique_ptr<If<Expanded>>>) {
                  return spf->codegen(context);
                } else if constexpr (std::is_same_v<
                                         SPFType,
                                         std::unique_ptr<Let<Expanded>>>) {
                  return spf->codegen(context);
                } else if constexpr (std::is_same_v<
                                         SPFType,
                                         std::unique_ptr<Setq<Expanded>>>) {
                  return spf->codegen(context);
                } else if constexpr (std::is_same_v<
                                         SPFType,
                                         std::unique_ptr<Goto<Expanded>>>) {
                  return spf->codegen(context);
                } else if constexpr (std::is_same_v<
                                         SPFType,
                                         std::unique_ptr<Go<Expanded>>>) {
                  spf->codegen(context);
                  return {};
                } else if constexpr (std::is_same_v<
                                         SPFType,
                                         std::unique_ptr<Def<Expanded>>>) {
                  return spf->load(context);
                } else if constexpr (std::is_same_v<
                                         SPFType,
                                         std::unique_ptr<Lambda<Expanded>>>) {
                  return spf->codegen(context);
                } else {
                  throw std::runtime_error(
                      "Unsupported special form encountered");
                }
              },
              expr_variant);
        } else {
          throw std::runtime_error("Unsupported expression encountered");
        }
      },
      expr);
}