#pragma once
#include "llvmincludes.h"
#include <variant>

/*
 * VarInst - Variable instance reference wrapper
 *
 * Encapsulates a reference to either a local variable (AllocaInst)
 * or a global variable (GlobalVariable) to provide uniform access.
 * This abstraction allows the compiler to handle different variable
 * storage types without exposing the implementation details.
 */
struct VarInst {
  /*
   * Construct a VarInst from a local variable allocation
   *
   * @param alloca - LLVM allocation instruction pointer for a local variable
   */
  VarInst(llvm::AllocaInst *alloca);

  /*
   * Construct a VarInst from a global variable
   *
   * @param global - LLVM global variable pointer
   */
  VarInst(llvm::GlobalVariable *global);

  /*
   * Get the local variable allocation
   *
   * @return AllocaInst* - Pointer to LLVM allocation instruction
   * @throws std::bad_variant_access - If this VarInst doesn't contain a local
   * variable
   */
  [[nodiscard]] llvm::AllocaInst *getLocal() const;

  /*
   * Get the global variable reference
   *
   * @return GlobalVariable* - Pointer to LLVM global variable
   * @throws std::bad_variant_access - If this VarInst doesn't contain a global
   * variable
   */
  [[nodiscard]] llvm::GlobalVariable *getGlob() const;

private:
  std::variant<llvm::AllocaInst *, llvm::GlobalVariable *> val_;
};

/*
 * TaggedLLVMVal - Tagged value wrapper for LLVM values
 *
 * Distinguishes between regular LLVM values and function values
 * to allow specialized handling during code generation. This is
 * necessary because LLVM treats functions differently from other values.
 */
struct TaggedLLVMVal {
  /*
   * Construct an empty (null) TaggedLLVMVal
   */
  TaggedLLVMVal();

  /*
   * Construct a TaggedLLVMVal from a regular LLVM value
   *
   * @param val - LLVM value pointer
   */
  TaggedLLVMVal(llvm::Value *val);

  /*
   * Construct a TaggedLLVMVal from a function pointer
   *
   * @param fn - LLVM function pointer
   */
  TaggedLLVMVal(llvm::Function *fn);

  TaggedLLVMVal(llvm::CallInst *);

  TaggedLLVMVal(llvm::PHINode *);

  template <typename T> [[nodiscard]] T get() const {
    if constexpr (std::is_same_v<T, llvm::Value *>)
      return std::visit([](auto &&arg) -> llvm::Value * { return arg; }, val_);

    return std::get<T>(val_);
  }

  template <typename T> [[nodiscard]] bool is() const {
    return std::holds_alternative<T>(val_);
  }

private:
  std::variant<llvm::Value *, llvm::Function *, llvm::CallInst *,
               llvm::PHINode *>
      val_;
};

/*
 * CreateEntryBlockAlloca - Create an allocation instruction in the entry block
 *
 * Allocates a variable at the beginning of the function's entry block.
 * This ensures that all allocations occur before any other operations,
 * which is important for maintaining consistent stack layout and lifetime.
 *
 * @param currentFn - Function in which to create the allocation
 * @param type - LLVM type of the variable to allocate
 * @param VarName - Name of the variable for debugging
 * @return AllocaInst* - Pointer to the created allocation instruction
 */
llvm::AllocaInst *CreateEntryBlockAlloca(llvm::Function *currentFn,
                                         llvm::Type *type,
                                         const std::string &VarName);

/*
 * getBuiltInName - Generate internal name for built-in operations
 *
 * Creates a mangled name for built-in operations to prevent
 * name collisions with user-defined functions.
 *
 * @param name - Base name of the built-in operation
 * @return string - Mangled internal name with "__internal_op_" prefix
 */
std::string getBuiltInName(std::string &&name);

/*
 * getGlobalFnName - Generate internal name for closure functions
 *
 * Creates a mangled name for closure function implementations to
 * prevent name collisions with other functions.
 *
 * @param name - Base name of the function
 * @return string - Mangled internal name with "__closure_" prefix
 */
std::string getGlobalFnName(std::string &&name);
