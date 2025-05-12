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
private:
  std::vector<llvm::Constant *> initCtors_;

public:
  CodegenContext();

  struct IRGenContext {
    IRGenContext();

    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;
    llvm::Module module;
  };

  class Memorymanager {
  public:
    Memorymanager(CodegenContext &codegenContext);

    void prepareArena(CodegenContext &codegenContext);

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
    TypeRegistry(CodegenContext &context);
    enum class BuiltInType { Int, Cons, Fn };

    llvm::GlobalVariable *getType(BuiltInType type);

    llvm::GlobalVariable *createBuiltinTypeDescVar(BuiltInType type);

    int toKind(BuiltInType type);

    std::string &toStrName(BuiltInType type);

    llvm::Type *toLLVMType(BuiltInType type);

    void emitCheckType(llvm::Value *val, BuiltInType type);

    llvm::Value *unpackVal(llvm::Value *val, BuiltInType type);

    llvm::Value *packVal(llvm::Value *val, BuiltInType type);

    llvm::Value *checkAndUnpack(llvm::Value *val, BuiltInType type);

    llvm::FunctionType *getStdFnType(size_t args);

    void typeDebug(llvm::Value *val);

    llvm::PointerType *ptrType;
    llvm::IntegerType *i32Type;
    llvm::IntegerType *i64Type;
    llvm::StructType *typeDescType;
    llvm::StructType *valueType;
    llvm::StructType *consType;
    llvm::StructType *envType;
    llvm::StructType *closureType;

  private:
    CodegenContext *parent_;

    llvm::StructType *makeValueType(IRGenContext &irgc);

    std::unordered_map<BuiltInType, llvm::GlobalVariable *> builtInTypes_;
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

    void emitPendingClosure(llvm::Value *global, size_t idx);

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

  IRGenContext context;
  llvm::GlobalVariable *debug;
  llvm::GlobalVariable *debug2;
  TypeRegistry type_manager;
  Memorymanager memory_manager;
  SymbolTable lexenv;

  void addCtor(size_t priority, llvm::Function *ctor);

  void emitCtors();
};

llvm::Value *createClosurecall(CodegenContext &codegenContext,
                               llvm::Value *inst, std::vector<ObjPtr> &args);

namespace Type {
const auto Int = CodegenContext::TypeRegistry::BuiltInType::Int;
const auto Cons = CodegenContext::TypeRegistry::BuiltInType::Cons;
const auto Fn = CodegenContext::TypeRegistry::BuiltInType::Fn;
} // namespace Type