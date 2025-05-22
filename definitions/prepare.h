#pragma once
#include "Optimizer.h"
#include "ir1lisp.h"
#include "meta.h"
#include "objects.h"
#include "util.h"
#include <optional>

/*
 * prepareCMain - Generate the main function for the compiled Lisp program
 *
 * Creates the entry point function that initializes the runtime system
 * and executes the compiled Lisp code. This function sets up the
 * necessary runtime environment and calls any initialization routines.
 *
 * @param codegenContext - Context providing LLVM infrastructure and state
 * @return Value* - Pointer to the generated main function
 */
llvm::Value *prepareCMain(CodegenContext &codegenContext);

/*
 * emitBuiltIn - Template function for generating built-in operations
 *
 * Generic template that creates LLVM functions for built-in operations.
 * Handles type checking, unboxing of arguments, calling the operation,
 * and boxing of results. This provides a uniform interface for creating
 * primitive operations like arithmetic, comparisons, and list operations.
 *
 * @tparam Arity - Number of arguments the built-in function takes
 * @tparam Op - Type of the operation function object
 * @param codegenContext - Context providing LLVM infrastructure and type system
 * @param fnName - Name of the function to create
 * @param opFn - Function object that implements the actual operation
 * @param inTypes - Array of expected input types (optional for each parameter)
 * @param retType - Expected return type (optional, for boxing result)
 * @return Function* - Generated LLVM function implementing the built-in
 *
 * The generated function:
 * 1. Takes 'Arity' number of boxed value parameters
 * 2. Unpacks arguments according to inTypes (if specified)
 * 3. Calls opFn with the unpacked arguments
 * 4. Boxes the result according to retType (if specified)
 * 5. Returns the boxed result
 *
 * Example usage:
 *   emitBuiltIn<2>(
 *     codegenContext, getBuiltInName("*"),
 *     [](llvm::IRBuilder<> &builder, llvm::ArrayRef<llvm::Value *> a) {
 *       return builder.CreateMul(a[0], a[1], "multmp");
 *     },
 *     {Type::Int, Type::Int}, Type::Int);
 *
 *
 *  or
 *
 *  emitBuiltIn<1>(
 *     codegenContext, "car",
 *     [this](llvm::IRBuilder<> &builder, llvm::ArrayRef<llvm::Value *> a) {
 *       auto call = builder.CreateCall(getBuiltInFn(getBuiltInName("car")),
 *                                      {a[0]}, "car.ret");
 *       return call;
 *     },
 *     {Type::Cons}, std::nullopt);
 */
template <unsigned Arity, typename Op>
llvm::Function *emitBuiltIn(
    CodegenContext &codegenContext, llvm::StringRef fnName, Op opFn,
    std::array<std::optional<CodegenContext::TypeRegistry::BuiltInType>, Arity>
        inTypes,
    std::optional<CodegenContext::TypeRegistry::BuiltInType> retType) {
  auto &[context, builder, module] = codegenContext.context;

  auto ptrType = codegenContext.type_manager.ptrType;
  // Build FunctionType: (ptr,ptr,… Arity times) -> ptr
  llvm::SmallVector<llvm::Type *, Arity> params(Arity, ptrType);
  auto FT = llvm::FunctionType::get(ptrType, params, /*isVarArg=*/false);

  auto F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, fnName,
                                  module);
  F->addFnAttr(llvm::Attribute::AlwaysInline);

  unsigned idx = 0;
  for (auto &arg : F->args())
    arg.setName(("x" + std::to_string(idx++)));

  auto entry = llvm::BasicBlock::Create(context, "entry", F);
  builder.SetInsertPoint(entry);

  llvm::SmallVector<llvm::Value *, Arity> rawArgs;
  idx = 0;
  for (auto &arg : F->args()) {
    if (inTypes[idx]) {
      /* Type is specified, unpack and type-check the argument */
      auto unpacked = codegenContext.type_manager.checkAndUnpack(
          &arg, inTypes[idx].value());
      rawArgs.push_back(unpacked);
    } else {
      /* No type specified, pass boxed value directly */
      rawArgs.push_back(&arg);
    }
    idx++;
  }

  /* Call the operation function with unpacked arguments */
  llvm::Value *rawRes = opFn(builder, rawArgs);

  if (retType) {
    /* Return type specified, box the result */
    llvm::Value *boxed =
        codegenContext.type_manager.packVal(rawRes, retType.value());
    builder.CreateRet(boxed);
  } else {
    /* No return type specified, return raw value (already boxed) */
    builder.CreateRet(rawRes);
  }

  return F;
}

/*
 * emitPanic - Generate a panic/abort function for runtime errors
 *
 * Creates a function that can be called when the runtime encounters
 * an unrecoverable error. This function typically prints an error
 * message and terminates the program.
 *
 * @param codegenContext - Context providing LLVM infrastructure
 * @return Function* - Generated panic function
 */
llvm::Function *emitPanic(CodegenContext &codegenContext);

/*
 * emitCons - Generate the cons cell constructor function
 *
 * Creates a built-in function for constructing cons cells (pairs).
 * Takes two arguments and creates a new cons cell with those values
 * as the car and cdr respectively.
 *
 * Signature: (any, any) -> cons
 *
 * @param codegenContext - Context providing LLVM infrastructure and memory
 * management
 * @return Function* - Generated cons constructor function
 */
llvm::Function *emitCons(CodegenContext &codegenContext);

/*
 * emitCar - Generate the car (first element) accessor function
 *
 * Creates a built-in function for accessing the first element (car)
 * of a cons cell. Performs type checking to ensure the argument is
 * a cons cell before accessing its car field.
 *
 * Signature: (cons) -> any
 *
 * @param codegenContext - Context providing LLVM infrastructure and type system
 * @return Function* - Generated car accessor function
 */
llvm::Function *emitCar(CodegenContext &codegenContext);

/*
 * emitCdr - Generate the cdr (second element) accessor function
 *
 * Creates a built-in function for accessing the second element (cdr)
 * of a cons cell. Performs type checking to ensure the argument is
 * a cons cell before accessing its cdr field.
 *
 * Signature: (cons) -> any
 *
 * @param codegenContext - Context providing LLVM infrastructure and type system
 * @return Function* - Generated cdr accessor function
 */
llvm::Function *emitCdr(CodegenContext &codegenContext);
