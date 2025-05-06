#pragma once
#include "meta.h"
#include "util.h"

class Object {
public:
  virtual ~Object() {}

  virtual llvm::Value *codegen(CodegenContext &CodegenContext) = 0;
};

class Number : public Object {
public:
  explicit Number(int64_t value = 0) : value_(value) {}

  llvm::Value *codegen(CodegenContext &CodegenContext) override;

  [[nodiscard]] int64_t getValue() const { return value_; }

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

  Cell(std::unique_ptr<SyntaxObject> head, std::unique_ptr<SyntaxObject> tail)
      : head_(std::move(head)), tail_(std::move(tail)) {}

  template <size_t idx> std::unique_ptr<SyntaxObject> &get() {
    static_assert(idx < 2, "Wrong index");
    if constexpr (idx == 0)
      return head_;
    return tail_;
  }

  struct ListIterator {
    using iterator_category = std::forward_iterator_tag;
    using value_type = SyntaxObject;
    using reference = SyntaxObject &;
    using pointer = SyntaxObject *;
    using difference_type = std::ptrdiff_t;

    explicit ListIterator(SyntaxObject *node) : node_(node) {}

    reference operator*() const {
      auto &ret = std::get<Cell>(*node_);
      return *ret.head_;
    }
    pointer operator->() const { return std::addressof(*node_); }

    pointer getCell() { return node_; }

    bool operator==(ListIterator const &o) const { return node_ == o.node_; }
    bool operator!=(ListIterator const &o) const { return !(*this == o); }

    ListIterator &operator++() {
      if (!node_)
        throw std::runtime_error("increment past end");
      auto c = std::get_if<Cell>(node_);
      if (!c)
        throw std::runtime_error("not a proper list");
      node_ = c->get<1>().get();
      if (node_ && !std::holds_alternative<Cell>(*node_))
        throw std::runtime_error("not a proper list");
      return *this;
    }
    ListIterator operator++(int) {
      auto tmp = *this;
      ++*this;
      return tmp;
    }

  private:
    SyntaxObject *node_;
  };

  struct ListView {
    ListView(SyntaxObject *head, SyntaxObject *tail)
        : head_(head), tail_(tail) {}

    ListView(SyntaxObject *head) : head_(head), tail_(nullptr) {}
    SyntaxObject *head_;
    SyntaxObject *tail_;
    [[nodiscard]] ListIterator begin() const { return ListIterator(head_); }
    [[nodiscard]] ListIterator end() const { return ListIterator(tail_); }
  };

private:
  std::unique_ptr<SyntaxObject> head_;
  std::unique_ptr<SyntaxObject> tail_;
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
/*class Prototype : public Object {
public:
  Prototype(std::string name, std::vector<std::string> args)
      : name_(std::move(name)), args_(std::move(args)) {}

  [[nodiscard]] const std::string &getName() const { return name_; }

  llvm::Value *codegen(CodegenContext &CodegenContext) override;

private:
  std::string name_;
  std::vector<std::string> args_;
};*/

/// FunctionAST - This class represents a function definition itself.
class Function : public Object {
public:
  Function(std::string &&name, std::vector<std::string> &&args, ObjPtr body)
      : name_(std::move(name)), args_(std::move(args)), body_(std::move(body)) {
  }

  llvm::Value *codegen(CodegenContext &CodegenContext) override;

private:
  std::string name_;
  std::vector<std::string> args_;
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

class Goto : public Object {
public:
  Goto(std::vector<std::variant<ObjPtr, std::string>> &&body)
      : body_(std::move(body)) {}

  llvm::Value *codegen(CodegenContext &CodegenContext) override;

private:
  std::vector<std::variant<ObjPtr, std::string>> body_;
};

class Go : public Object {
public:
  Go(std::string &&tag) : tag_(tag) {}

  llvm::Value *codegen(CodegenContext &CodegenContext) override;

private:
  std::string tag_;
};

class Let : public Object {
  std::string name_;
  ObjPtr init_;
  ObjPtr body_;

public:
  Let(std::string &&name, ObjPtr init, ObjPtr body)
      : name_(std::move(name)), init_(std::move(init)), body_(std::move(body)) {
  }

  llvm::Value *codegen(CodegenContext &CodegenContext) override;
};
