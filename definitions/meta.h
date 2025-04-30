#pragma once
#include <algorithm>
#include <concepts>
#include <cstdint>
#include <functional>
#include <istream>
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
struct CodegenContext {
  CodegenContext();

  struct IRGenContext {
    IRGenContext();

    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;
    llvm::Module module;
  };

  class SymbolTable {
  public:
    SymbolTable(CodegenContext &codegenContext);

    void enterScope();

    void exitScope();

    bool isTopLevel();

    llvm::Function *getFn(const std::string &name);

    void addVar(const std::string &name, llvm::AllocaInst *inst);

    llvm::AllocaInst *lookUpVar(const std::string &name);

    std::vector<std::unordered_map<std::string, llvm::BasicBlock *>> &tagEnvs();

    std::unordered_map<std::string, llvm::BasicBlock *> &lastTagEnv();

    llvm::Function *getTrapFn();

  private:
    llvm::Function *trapFn_;

    std::unordered_map<std::string, llvm::GlobalVariable *> constantGlobals_;

    std::unordered_map<std::string, llvm::Function *> builtInFns_;

    std::vector<std::unordered_map<std::string, llvm::AllocaInst *>>
        named_values_;

    std::vector<std::unordered_map<std::string, llvm::BasicBlock *>> tagEnvs_;

    CodegenContext *parent_;
  };

  class Memorymanager {
  public:
    Memorymanager(IRGenContext &irgc);

    void prepareArena(IRGenContext &irgc);

    void munmapArena(IRGenContext &irgc);

    llvm::Function *getmmapFn();

    llvm::Function *getmunmapFn();

    llvm::Function *getTrapFn();

    llvm::Function *getArenaAllocator();

    llvm::GlobalVariable *getArenaPtrGV();

    llvm::GlobalVariable *getArenaNextGV();

    llvm::GlobalVariable *getArenaSizeGV();

  private:
    llvm::Function *defineArenaAlloc(IRGenContext &irgc);

    llvm::Function *mmapFn_;
    llvm::Function *munmapFn_;
    llvm::GlobalVariable *arenaPtrGV_;
    llvm::GlobalVariable *arenaNextGV_;
    llvm::GlobalVariable *arenaSizeGV_;
    llvm::Function *arenaAllocValueFn_;
  };

  class TypeRegistry {
  public:
    TypeRegistry(IRGenContext &irgc);

    llvm::GlobalVariable *getType(const std::string &name);

    llvm::StructType *getConsTy();

    llvm::StructType *getTypeDescTy();

    llvm::StructType *getValueTy();

    llvm::PointerType *getPtrType();

    llvm::GlobalVariable *createBuiltinTypeDescVar(IRGenContext &irgc,
                                                   llvm::StringRef name,
                                                   int kind);

  private:
    llvm::StructType *makeTypeDescType(IRGenContext &irgc);

    llvm::StructType *makeValueType(IRGenContext &irgc,
                                    llvm::StructType *TypeDescTy);

    llvm::StructType *makeConsType(IRGenContext &irgc);

    llvm::StructType *typeDescTy_;
    llvm::StructType *valueTy_;
    llvm::PointerType *ptrTy_;
    llvm::StructType *consTy_;
    std::unordered_map<std::string, llvm::GlobalVariable *> builtInTypes_;
  };

  IRGenContext context;
  Memorymanager memory_manager;
  TypeRegistry type_manager;
  SymbolTable lexenv;
};
