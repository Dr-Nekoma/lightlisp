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

#include "llvmincludes.h"
#include "util.h"

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

using ObjPtr = std::unique_ptr<Object>;

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

    llvm::Function *getBuiltInFn(const std::string &name);

    void addVar(const std::string &name, llvm::AllocaInst *inst);

    void addVar(const std::string &name, llvm::GlobalVariable *inst);

    void addFreeVar(const std::string &name, llvm::AllocaInst *var);

    std::vector<std::pair<std::string, llvm::AllocaInst *>> popFreeVars();

    std::pair<VarInst, bool> lookUpVar(const std::string &name);

    size_t freeVarsSize();

    llvm::Value *getCurrentEnv();

    std::vector<std::unordered_map<std::string, llvm::BasicBlock *>> &tagEnvs();

    std::unordered_map<std::string, llvm::BasicBlock *> &lastTagEnv();

    llvm::Function *getTrapFn();

    void addPendingClosure(llvm::Function *fnPtr, const std::string &fnName,
                           std::vector<llvm::AllocaInst *> &&freeVars,
                           size_t arity);

    void emitPendingClosure(llvm::GlobalVariable *global, size_t idx);

    void initGlobalCtors();

  private:
    struct PendingClosures {
      PendingClosures(CodegenContext &codegenContext, llvm::Function *fnPtr,
                      const std::string &fnName,
                      std::vector<llvm::AllocaInst *> &&freeVars, size_t arity);

      llvm::Function *fnPtr_;
      std::string fnName_;
      std::vector<llvm::AllocaInst *> freeVars_;
      size_t arity_;
      llvm::GlobalVariable *fnGlobal_;
    };

    llvm::Function *trapFn_;

    std::unordered_map<std::string, llvm::GlobalVariable *> constantGlobals_;

    std::unordered_map<std::string, llvm::Function *> builtInFns_;

    std::vector<std::unordered_map<std::string, llvm::AllocaInst *>>
        named_values_;

    std::unordered_map<std::string, llvm::GlobalVariable *> globals_;

    std::vector<std::unordered_map<std::string, llvm::BasicBlock *>> tagEnvs_;

    std::vector<std::vector<std::pair<std::string, llvm::AllocaInst *>>>
        freeVars_;

    std::vector<llvm::Value *> envStack_;

    std::vector<PendingClosures> pendingClosures_;

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

    llvm::StructType *getEnvTy();

    llvm::StructType *getClosureTy();

    llvm::GlobalVariable *createBuiltinTypeDescVar(IRGenContext &irgc,
                                                   llvm::StringRef name,
                                                   int kind);

  private:
    llvm::StructType *makeTypeDescType(IRGenContext &irgc);

    llvm::StructType *makeValueType(IRGenContext &irgc,
                                    llvm::StructType *TypeDescTy);

    llvm::StructType *makeConsType(IRGenContext &irgc);

    llvm::StructType *makeEnvType(IRGenContext &irgc);

    llvm::StructType *makeClosureType(IRGenContext &irgc);

    llvm::StructType *typeDescTy_;
    llvm::StructType *valueTy_;
    llvm::PointerType *ptrTy_;
    llvm::StructType *consTy_;
    llvm::StructType *envTy_;
    llvm::StructType *closureTy_;
    std::unordered_map<std::string, llvm::GlobalVariable *> builtInTypes_;
  };

  IRGenContext context;
  Memorymanager memory_manager;
  TypeRegistry type_manager;
  SymbolTable lexenv;
};

llvm::Value *createClosurecall(CodegenContext &codegenContext,
                               llvm::Value *inst, std::vector<ObjPtr> &args);