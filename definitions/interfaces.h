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

class CodegenContext {
public:
  CodegenContext()
      : context_(std::make_unique<llvm::LLVMContext>()),
        builder_(std::make_unique<llvm::IRBuilder<>>(*context_)),
        module_(std::make_unique<llvm::Module>("my lisp", *context_)) {}

  // Getter methods
  llvm::LLVMContext &context() { return *context_; }

  llvm::IRBuilder<> &builder() { return *builder_; }

  llvm::Module &module() { return *module_; }

  std::map<std::string, llvm::AllocaInst *> &named_values() {
    return named_values_;
  }

private:
  std::unique_ptr<llvm::LLVMContext> context_;
  std::unique_ptr<llvm::IRBuilder<>> builder_;
  std::unique_ptr<llvm::Module> module_;
  std::map<std::string, llvm::AllocaInst *> named_values_;
};

class Object {
public:
  virtual ~Object() {}

  //[[nodiscard]] virtual Types ID() const { return Types::t; }

  //[[nodiscard]] virtual bool IsFalse() const { return
  // false; }

  virtual llvm::Value *codegen(CodegenContext &CodegenContext) = 0;

  // virtual void Walk(const std::function<void(ObjPtr *)> &fn) { return; };
};

using ObjPtr = std::shared_ptr<Object>;

class Number : public Object {
public:
  explicit Number(int64_t value = 0) : value_(value) {}

  llvm::Value *codegen(CodegenContext &CodegenContext) override;

private:
  int64_t value_;
};

class Symbol {
public:
  explicit Symbol(std::string value) : value_(std::move(value)) {}

  const std::string &getName() { return value_; }

private:
  std::string value_;
};

class Cell {
public:
  Cell() : head_(nullptr), tail_(nullptr) {}

  Cell(std::shared_ptr<SyntaxObject> head, std::shared_ptr<SyntaxObject> tail)
      : head_(head), tail_(tail) {}

  template <size_t idx> std::shared_ptr<SyntaxObject> &get() {
    static_assert(idx < 2, "Wrong index");
    if constexpr (idx == 0)
      return head_;
    return tail_;
  }

private:
  std::shared_ptr<SyntaxObject> head_;
  std::shared_ptr<SyntaxObject> tail_;
};

/// VariableObject - Expression class for referencing a variable, like "a".
class Variable : public Object {
public:
  Variable(std::string name) : name_(std::move(name)) {}

  llvm::Value *codegen(CodegenContext &CodegenContext) override;

  [[nodiscard]] const std::string &getName() const { return name_; }

private:
  const std::string name_;
};

/// CallObject - Expression class for function calls.
class Call : public Object {
public:
  Call(std::string callee, std::vector<ObjPtr> args)
      : callee_(std::move(callee)), args_(std::move(args)) {}

  llvm::Value *codegen(CodegenContext &CodegenContext) override;

private:
  std::string callee_;
  std::vector<ObjPtr> args_;
};

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
class Prototype : public Object {
public:
  Prototype(std::string name, std::vector<std::string> args)
      : name_(std::move(name)), args_(std::move(args)) {}

  [[nodiscard]] const std::string &getName() const { return name_; }

  llvm::Function *codegen(CodegenContext &CodegenContext) override;

private:
  std::string name_;
  std::vector<std::string> args_;
};

/// FunctionAST - This class represents a function definition itself.
class Function : public Object {
public:
  Function(std::unique_ptr<Prototype> proto, ObjPtr body)
      : proto_(std::move(proto)), body_(std::move(body)) {}

  llvm::Function *codegen(CodegenContext &CodegenContext) override;

private:
  std::unique_ptr<Prototype> proto_;
  ObjPtr body_;
};

class BuiltInOp : public Object {
public:
  BuiltInOp(std::string name, ObjPtr fst, ObjPtr snd)
      : name_(std::move(name)), fst_(std::move(fst)), snd_(std::move(snd)) {}

  [[nodiscard]] const std::string &getName() const { return name_; }

  llvm::Value *codegen(CodegenContext &CodegenContext) override;

private:
  std::string name_;
  ObjPtr fst_;
  ObjPtr snd_;
};

class If : public Object {
public:
  If(ObjPtr Cond, ObjPtr Then, ObjPtr Else)
      : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}

  llvm::Value *codegen(CodegenContext &CodegenContext) override;

private:
  ObjPtr Cond, Then, Else;
};

/*
class Goto : public Object {

public:
  Goto(const std::string &VarName, std::unique_ptr<Object> Start,
       std::unique_ptr<Object> End, std::unique_ptr<Object> Step,
       std::unique_ptr<Object> Body)
      : VarName(VarName), Start(std::move(Start)), End(std::move(End)),
        Step(std::move(Step)), Body(std::move(Body)) {}

  llvm::Value *codegen(CodegenContext &CodegenContext) override;

private:
  std::string name_;
  ObjPtr Start, End, Step, Body;
};
*/
/*
/// VarObject - Expression class for var/in
class VarObject : public Object {
  std::vector<std::pair<std::string, std::unique_ptr<Object>>> VarNames;
  std::unique_ptr<Object> Body;

public:
  VarObject(
      std::vector<std::pair<std::string, std::unique_ptr<Object>>> VarNames,
      std::unique_ptr<Object> Body)
      : VarNames(std::move(VarNames)), Body(std::move(Body)) {}

  llvm::Value *codegen(CodegenContext &CodegenContext) override;
};
*/