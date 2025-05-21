#include "Optimizer.h"
#include "meta.h"
#include "objects.h"
#include "util.h"

/*
 * Number::codegen - Generate LLVM IR for integer literals
 *
 * Converts a Lisp numeric literal to LLVM IR by:
 * 1. Creating a 64-bit integer constant
 * 2. Boxing the raw value with appropriate type tagging
 *
 * @param codegenContext - Provides LLVM context, builder, module, and type
 * management
 * @return TaggedLLVMVal - Boxed integer value as LLVM IR
 */
TaggedLLVMVal Number::codegen(CodegenContext &codegenContext) {
  auto &[context, builder, module] = codegenContext.context;
  llvm::Value *raw = builder.getInt64(value_);

  auto name = "num" + std::to_string(value_);
  llvm::Value *boxed = codegenContext.type_manager.packVal(raw, Type::Int);

  return boxed;
}

/*
 * Variable::codegen - Generate LLVM IR for variable references
 *
 * Resolves variable references by:
 * 1. Checking if the name refers to a built-in function
 * 2. Looking up the variable in the symbol table
 * 3. Loading the value based on scope (local, captured, global)
 *
 * For captured variables, updates closure environment tracking information.
 *
 * @param codegenContext - Provides LLVM context, builder, module, and lexical
 * environment
 * @return TaggedLLVMVal - Referenced variable value as LLVM IR
 * @throws std::runtime_error - If variable not found
 */
TaggedLLVMVal Variable::codegen(CodegenContext &codegenContext) {
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
  case CodegenContext::SymbolTable::VarStatus::NotFound:
    throw std::runtime_error("Unknown variable name");
  }
}

/*
 * Call::codegen - Generate LLVM IR for function calls and closure invocations
 *
 * Handles two types of callables:
 * 1. Direct function calls (built-in or previously compiled functions)
 * 2. User-defined closures requiring environment handling
 *
 * For direct functions, validates argument count and generates call
 * instruction. For closures, unpacks the closure object and invokes via
 * createClosurecall().
 *
 * @param codegenContext - Provides LLVM context, builder, module, and type
 * management
 * @return TaggedLLVMVal - Function call result as LLVM IR
 * @throws std::runtime_error - If argument count mismatch
 */
TaggedLLVMVal Call::codegen(CodegenContext &codegenContext) {
  auto &[context, builder, module] = codegenContext.context;
  // Look up the name in the global module table.
  TaggedLLVMVal CalleeF = callee_->codegen(codegenContext);
  if (CalleeF.isFn()) {
    auto fn = CalleeF.getFn();
    // If argument mismatch error.
    if (fn->arg_size() != args_.size())
      throw std::runtime_error("Incorrect # arguments passed");

    std::vector<llvm::Value *> ArgsV;
    for (auto &Arg : args_) {
      ArgsV.push_back(Arg->codegen(codegenContext).get());
      if (!ArgsV.back())
        return {};
    }

    auto call = builder.CreateCall(fn, ArgsV, "calltmp");
    // if (callee_ == "car" || callee_ == "cdr") { // FIXME this can't be good
    //   call->setOnlyReadsMemory();
    // }

    return call;
  }
  auto userVar = CalleeF.get();
  auto closure = codegenContext.type_manager.checkAndUnpack(userVar, Type::Fn);

  return createClosurecall(codegenContext, closure, args_);
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

/*
 * Function::codegen - Generate LLVM IR for function definitions
 *
 * Creates a function that can capture its lexical environment (closure):
 * 1. Creates an LLVM function with environment pointer as first argument
 * 2. Sets up entry block and argument allocations
 * 3. Generates code for function body within a new lexical scope
 * 4. Records captured variables for closure creation
 * 5. Registers the closure constructor
 *
 * @param codegenContext - Provides LLVM context, builder, module, and lexical
 * environment
 * @return TaggedLLVMVal - Function as LLVM IR
 */
TaggedLLVMVal Function::codegen(CodegenContext &codegenContext) {
  // First, check for an existing function from a previous 'extern' declaration.
  /*llvm::Function *currentFn =
      codegenContext.context.module.getFunction(proto_.getName());

  if (!currentFn)
    currentFn = proto_.codegen(codegenContext);

  if (!currentFn)
    return nullptr;

  if (!currentFn->empty())
    throw std::runtime_error("Function cannot be redefined.");*/
  auto ptrType = codegenContext.type_manager.ptrType;
  auto &[context, builder, module] = codegenContext.context;
  auto size = args_.size();
  llvm::FunctionType *FT = codegenContext.type_manager.getStdFnType(size);

  llvm::Function *F =
      llvm::Function::Create(FT, llvm::Function::InternalLinkage,
                             getGlobalFnName(std::string(name_)), module);

  // First argument is always the environment pointer for closures
  auto &arg = *F->arg_begin();
  arg.setName("_____env");

  unsigned Idx = 0;
  auto it1 = F->arg_begin(); // ugly, FIXME
  it1++;
  for (; it1 != F->arg_end(); it1++)
    it1->setName(args_[Idx++]);

  // Create a new basic block to start insertion into.
  llvm::BasicBlock *BB = llvm::BasicBlock::Create(context, "entry", F);
  builder.SetInsertPoint(BB);

  codegenContext.lexenv.enterScope(&*F->arg_begin());
  auto it = F->arg_begin();
  it++;
  for (; it != F->arg_end(); it++) {
    llvm::AllocaInst *Alloca =
        CreateEntryBlockAlloca(F, ptrType, std::string(it->getName()));

    builder.CreateStore(&*it, Alloca);

    codegenContext.lexenv.addVar(std::string(it->getName()), Alloca);
  }

  if (llvm::Value *RetVal = body_->codegen(codegenContext).get()) {
    builder.CreateRet(RetVal);

    codegenContext.lexenv.exitScope(true);

    llvm::verifyFunction(*F);

    // Optimizer::getFPM().run(*F, Optimizer::getFAM());
    auto freeVarsAndNames = codegenContext.lexenv.popFreeVars();

    std::vector<llvm::AllocaInst *> freeVars;
    freeVars.reserve((freeVarsAndNames.size()));
    for (auto &[_, inst] : freeVarsAndNames) {
      freeVars.push_back(inst);
    }

    codegenContext.lexenv.addClosureCtor(F, name_, std::move(freeVars),
                                         args_.size());

    return F;
  }

  codegenContext.lexenv.exitScope(true);
  // Error reading body, remove function.
  F->eraseFromParent();
  return {};
}

/*
 * Setq::codegen - Generate LLVM IR for variable assignment
 *
 * Implements variable assignment based on variable scope:
 * 1. For local variables - direct store to allocation
 * 2. For captured variables - update in closure environment
 * 3. For global variables - update global storage
 *
 * @param codegenContext - Provides LLVM context, builder, module, and lexical
 * environment
 * @return TaggedLLVMVal - Assigned value (right-hand side) as LLVM IR
 * @throws std::runtime_error - If variable not found
 */
TaggedLLVMVal Setq::codegen(CodegenContext &codegenContext) {
  // Special case '=' because we don't want to emit the LHS as an expression.
  auto &[context, builder, module] = codegenContext.context;

  auto name = fst_->getName();
  auto [var, status] = codegenContext.lexenv.lookUpVar(name);

  llvm::Value *Val = snd_->codegen(codegenContext).get();
  // Codegen the RHS.
  if (!Val)
    return {};

  switch (status) {
  case CodegenContext::SymbolTable::VarStatus::Local:
    builder.CreateStore(Val, var.getLocal());
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

    codegenContext.type_manager.copyValInto(Val, val);
    break;
  }
  case CodegenContext::SymbolTable::VarStatus::Global:
    builder.CreateStore(Val, var.getGlob());
    break;
  case CodegenContext::SymbolTable::VarStatus::NotFound:
    throw std::runtime_error("Unknown variable name");
  }

  return Val;
}

/*
 * If::codegen - Generate LLVM IR for conditional expressions
 *
 * Implements if-then-else control flow:
 * 1. Generates and unpacks condition value
 * 2. Creates basic blocks for then, else, and merge points
 * 3. Generates code for both branches
 * 4. Uses PHI node to select the correct result value
 *
 * @param codegenContext - Provides LLVM context, builder, module
 * @return TaggedLLVMVal - Result of selected branch as LLVM IR
 */
TaggedLLVMVal If::codegen(CodegenContext &codegenContext) {
  auto &[context, builder, module] = codegenContext.context;

  // Generate the boxed Value* for the condition
  llvm::Value *condBoxed = Cond->codegen(codegenContext).get();
  if (!condBoxed)
    return {};

  auto rawCond =
      codegenContext.type_manager.checkAndUnpack(condBoxed, Type::Int);

  // Compare i64 != 0 → i1
  llvm::Value *condI1 =
      builder.CreateICmpNE(rawCond, builder.getInt64(0), "ifcond");

  llvm::Function *currentFn = builder.GetInsertBlock()->getParent();
  llvm::BasicBlock *ThenBB =
      llvm::BasicBlock::Create(context, "then", currentFn);
  llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(context, "else");
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(context, "ifcont");

  builder.CreateCondBr(condI1, ThenBB, ElseBB);

  builder.SetInsertPoint(ThenBB);

  llvm::Value *ThenV = Then->codegen(codegenContext).get();
  if (!ThenV)
    return {};

  builder.CreateBr(MergeBB);
  // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
  ThenBB = builder.GetInsertBlock();

  // Emit else block.
  currentFn->insert(currentFn->end(), ElseBB);
  builder.SetInsertPoint(ElseBB);

  llvm::Value *ElseV = Else->codegen(codegenContext).get();
  if (!ElseV)
    return {};

  builder.CreateBr(MergeBB);
  // codegen of 'Else' can change the current block, update ElseBB for the PHI.
  ElseBB = builder.GetInsertBlock();

  currentFn->insert(currentFn->end(), MergeBB);
  builder.SetInsertPoint(MergeBB);
  llvm::PHINode *PN =
      builder.CreatePHI(codegenContext.type_manager.ptrType, 2, "iftmp");

  PN->addIncoming(ThenV, ThenBB);
  PN->addIncoming(ElseV, ElseBB);
  return PN;
}

/*
 * Goto::codegen - Generate LLVM IR for tagbody construct
 *
 * Implements the Lisp tagbody construct that allows non-local jumps:
 * 1. Creates basic blocks for each tag
 * 2. Sets up storage to track the last evaluated expression
 * 3. Generates code for expressions in body, interleaved with tag points
 * 4. Preserves the value of the last evaluated expression
 *
 * @param codegenContext - Provides LLVM context, builder, module, and lexical
 * environment
 * @return TaggedLLVMVal - Value of last evaluated expression in body
 */
TaggedLLVMVal Goto::codegen(CodegenContext &codegenContext) {
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
      auto &expr = std::get<ObjPtr>(item);
      ret = expr->codegen(codegenContext).get();
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
      builder.SetInsertPoint(codegenContext.lexenv.lastTagEnv()[*tag]);
    } else {
      auto &expr = std::get<ObjPtr>(item);
      auto v = expr->codegen(codegenContext).get();
      if (v)
        builder.CreateStore(v, lastVal);
      curBB = builder.GetInsertBlock();
    }
  }

  if (!curBB->getTerminator())
    builder.CreateBr(afterBB);
  builder.SetInsertPoint(afterBB);
  llvm::Value *last = builder.CreateLoad(codegenContext.type_manager.ptrType,
                                         lastVal, "tagbody.last");
  codegenContext.lexenv.tagEnvs().pop_back();

  return last;
}

/*
 * Go::codegen - Generate LLVM IR for go expression (jump to tag)
 *
 * Implements jumps to tags defined in enclosing tagbody constructs:
 * 1. Searches for target tag in lexical environment
 * 2. Creates unconditional branch to the tag's basic block
 *
 * @param codegenContext - Provides LLVM context, builder, module, and lexical
 * environment
 * @return TaggedLLVMVal - Empty value (control flow never returns)
 * @throws std::runtime_error - If target tag not found
 */
TaggedLLVMVal Go::codegen(CodegenContext &codegenContext) {
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

  return {};
}

/*
 * Let::codegen - Generate LLVM IR for local variable binding
 *
 * Implements local variable binding with initialization:
 * 1. Generates code for initializer expression
 * 2. Allocates storage for local variable
 * 3. Creates new lexical scope with the bound variable
 * 4. Generates code for body expression in new scope
 * 5. Restores previous lexical scope
 *
 * @param codegenContext - Provides LLVM context, builder, module, and lexical
 * environment
 * @return TaggedLLVMVal - Result of body expression as LLVM IR
 */
TaggedLLVMVal Let::codegen(CodegenContext &codegenContext) {
  auto &[context, builder, module] = codegenContext.context;
  llvm::Function *currentFn;
  llvm::BasicBlock *previousBB = builder.GetInsertBlock();
  if (codegenContext.lexenv.isTopLevel()) {
    currentFn = codegenContext.lexenv.getCtorFn();
    builder.SetInsertPoint(codegenContext.lexenv.getCtorBlock());
  }
  currentFn = builder.GetInsertBlock()->getParent();

  // Emit the initializer before adding the variable to scope, this prevents
  // the initializer from referencing the variable itself.
  llvm::Value *initVal = init_->codegen(codegenContext).get();
  if (!initVal)
    return {};

  auto Alloca = CreateEntryBlockAlloca(
      currentFn, codegenContext.type_manager.ptrType, name_);
  builder.CreateStore(initVal, Alloca);

  // Remember the old variable binding so that we can restore the binding when
  // we unrecurse.
  codegenContext.lexenv.enterScope();

  // Remember this binding.
  codegenContext.lexenv.addVar(name_, Alloca);

  builder.SetInsertPoint(previousBB);
  // Codegen the body, now that all vars are in scope.
  llvm::Value *BodyVal = body_->codegen(codegenContext).get();
  if (!BodyVal)
    return {};

  // Pop our variable from scope.
  codegenContext.lexenv.exitScope(false);

  // Return the body computation.
  return BodyVal;
}