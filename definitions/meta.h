#pragma once
#include <algorithm>
#include <concepts>
#include <cstdint>
#include <functional>
#include <istream>
#include <map>
#include <memory>
#include <stdexcept>
#include <string>
#include <string_view>
#include <sys/stat.h>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"

#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"

#include <llvm/IR/Verifier.h>

#include "types.h"

class Object;
// class Scope;
class Number;  // constant
class Symbol;  // constant
class Boolean; // constant ?
class Function;
class String;
class LambdaFunction;
class Cell;
class SpecialForm;
// class LispError;
// class RuntimeError;
// class SyntaxError;

using SyntaxObject = std::variant<Number, Symbol, Cell>;

using IntOpFn = std::function<llvm::Value *(llvm::IRBuilder<> &, llvm::Value *,
                                            llvm::Value *)>;
class CodegenContext {
public:
  CodegenContext();

  // Getter methods
  llvm::LLVMContext &context();

  llvm::IRBuilder<> &builder();

  llvm::Module &module();

  std::map<std::string, llvm::AllocaInst *> &named_values();

  std::vector<std::unordered_map<std::string, llvm::BasicBlock *>> &tagEnvs();

  llvm::StructType *getTypeDescTy();

  llvm::StructType *getValueTy();

  llvm::PointerType *getPtrType();

  llvm::Function *getmmapFn();

  llvm::Function *getmunmapFn();

  llvm::Function *getTrapFn();

  llvm::Function *getArenaAllocator();

  llvm::GlobalVariable *getArenaPtrGV();

  llvm::GlobalVariable *getArenaNextGV();

  llvm::GlobalVariable *getArenaSizeGV();

  std::unordered_map<std::string, llvm::BasicBlock *> &lastTagEnv();

  llvm::Function *getFn(const std::string &name);

  void addType(const std::string &name, llvm::GlobalVariable *type);

  llvm::GlobalVariable *getType(const std::string &name);

  llvm::StructType *getConsTy();

private:
  void defineArenaAlloc();

  std::unique_ptr<llvm::LLVMContext> context_;
  std::unique_ptr<llvm::IRBuilder<>> builder_;
  std::unique_ptr<llvm::Module> module_;
  std::map<std::string, llvm::AllocaInst *> named_values_;
  std::vector<std::unordered_map<std::string, llvm::BasicBlock *>> tagEnvs_;

  llvm::StructType *typeDescTy_;
  llvm::StructType *valueTy_;
  llvm::PointerType *ptrTy_;

  llvm::StructType *consTy_;

  llvm::Function *mmapFn_ = nullptr;
  llvm::Function *munmapFn_ = nullptr;
  llvm::Function *trapFn_ = nullptr;
  llvm::GlobalVariable *arenaPtrGV_ = nullptr;
  llvm::GlobalVariable *arenaNextGV_ = nullptr;
  llvm::GlobalVariable *arenaSizeGV_ = nullptr;
  llvm::Function *arenaAllocValueFn_ = nullptr;

  std::unordered_map<std::string, llvm::Function *> builtInFns_;

  std::unordered_map<std::string, llvm::GlobalVariable *> builtInTypes_;
};
