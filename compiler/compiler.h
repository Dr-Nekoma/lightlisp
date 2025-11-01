#pragma once

#include "ir1lisp.h"
#include "ir2lisp.h"
#include "meta.h"
#include "util.h"

/*
 * Number - AST node for integer literals
 *
 * Represents literal integer values in the Lisp source code.
 * Stores the numeric value and generates LLVM IR for boxed integers.
 */

class Number {
public:
  /*
   * Construct a Number node with the given integer value
   *
   * @param value - Integer value (defaults to 0)
   */
  explicit Number(int64_t value) : value_(value) {}

  /*
   * Number::codegen - Generate LLVM IR for integer literals
   *
   * Converts a Lisp numeric literal to LLVM IR by:
   * 1. Creating a 64-bit integer constant
   * 2. Boxing the raw value with appropriate type tagging
   *
   * @param codegenContext - Provides LLVM context, builder, module, and type
   * management
   * @return llvm::Value* - Boxed integer value as LLVM IR
   */
  llvm::Value *codegen(CodegenContext &CodegenContext);

  /*
   * Get the numeric value of this literal
   *
   * @return int64_t - The integer value
   */
  [[nodiscard]] int64_t getValue() const { return value_; }

private:
  int64_t value_; // The stored integer value
};

/*
 * Symbol - Represents symbolic names in Lisp syntax
 *
 * Stores symbolic identifiers used in parsing. These are converted
 * to Symbol nodes during semantic analysis for variable references
 * or used directly for special forms and function names.
 */
/*
 * Symbol - AST node for variable references
 *
 * Represents references to variables in the Lisp code. Handles
 * variable lookup through different scopes (local, captured, global)
 * and generates appropriate load instructions.
 */
class Symbol {
public:
  /*
   * Construct a Symbol node with the given name
   *
   * @param name - Name of the variable to reference
   */
  explicit Symbol(std::string name) : name_(std::move(name)) {}

  /*
   * Symbol::codegen - Generate LLVM IR for variable references
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
   * @return llvm::Value* - Referenced variable value as LLVM IR
   * @throws std::runtime_error - If variable not found
   */
  TaggedLLVMVal load(CodegenContext &CodegenContext);

  /*
   * Get the variable name
   *
   * @return const string& - Reference to the variable name
   */
  [[nodiscard]] const std::string &getName() const { return name_; }

private:
  const std::string name_; // Name of the variable
};

/*
 * Cell - Represents Lisp cons cells (pairs)
 *
 * Implements the fundamental Lisp data structure for building lists
 * and trees. Each cell contains a head (car) and tail (cdr) element.
 * Provides iterator support for treating cells as linked lists.
 */
class Cell {
public:
  /*
   * Default constructor creates an empty cell
   */
  Cell();

  /*
   * Construct a cell with head and tail elements
   *
   * @param head - First element of the pair (car)
   * @param tail - Second element of the pair (cdr)
   */
  Cell(std::unique_ptr<SyntaxObject> head, std::unique_ptr<SyntaxObject> tail);

  /*
   * Template accessor for cell elements
   *
   * @tparam idx - Index (0 for head, 1 for tail)
   * @return unique_ptr<SyntaxObject>& - Reference to the element
   */
  template <size_t idx> std::unique_ptr<SyntaxObject> &get() {
    static_assert(idx < 2, "Wrong index");
    if constexpr (idx == 0)
      return head_; /* Return head (car) for index 0 */
    return tail_;   /* Return tail (cdr) for index 1 */
  }

  /*
   * ListIterator - Forward iterator for traversing cons cell lists
   */
  struct ListIterator {
    using iterator_category = std::forward_iterator_tag;
    using value_type = SyntaxObject;
    using reference = SyntaxObject &;
    using pointer = SyntaxObject *;
    using difference_type = std::ptrdiff_t;

    /*
     * Construct iterator pointing to the given node
     *
     * @param node - Pointer to current list node
     */
    explicit ListIterator(SyntaxObject *node);

    /*
     * Dereference operator - get the head of current cell
     *
     * @return reference - Reference to the head element
     */
    reference operator*() const;

    /*
     * Arrow operator for member access
     *
     * @return pointer - Pointer to current node
     */
    pointer operator->() const;

    /*
     * Get the current cell pointer
     *
     * @return pointer - Pointer to current node
     */
    pointer getCell();

    /*
     * Equality comparison
     *
     * @param o - Other iterator to compare with
     * @return bool - True if iterators point to same node
     */
    bool operator==(ListIterator const &o) const;

    /*
     * Inequality comparison
     *
     * @param o - Other iterator to compare with
     * @return bool - True if iterators point to different nodes
     */
    bool operator!=(ListIterator const &o) const;

    /*
     * Pre-increment operator - advance to next cell
     *
     * Follows the cdr pointer to the next cell in the list.
     *
     * @return ListIterator& - Reference to this iterator
     * @throws std::runtime_error - If incrementing past end or improper list
     */
    ListIterator &operator++();

    /*
     * Post-increment operator
     *
     * @return ListIterator - Copy of iterator before increment
     */
    ListIterator operator++(int);

  private:
    SyntaxObject *node_; /* Current position in the list */
  };

  /*
   * ListView - Range-based view of a cons cell list
   *
   * Provides begin/end iterators for range-based for loops
   * over cons cell lists.
   */
  struct ListView {
    /*
     * Construct a view with explicit head and tail
     *
     * @param head - Start of the list
     * @param tail - End of the list (usually nullptr)
     */
    ListView(SyntaxObject *head, SyntaxObject *tail);

    /*
     * Construct a view with just the head (tail defaults to nullptr)
     *
     * @param head - Start of the list
     */
    ListView(SyntaxObject *head);

    SyntaxObject *head_; /* Start of the list */
    SyntaxObject *tail_; /* End of the list */

    /*
     * Get iterator to the beginning of the list
     *
     * @return ListIterator - Iterator pointing to first element
     */
    [[nodiscard]] ListIterator begin() const;

    /*
     * Get iterator to the end of the list
     *
     * @return ListIterator - Iterator pointing past last element
     */
    [[nodiscard]] ListIterator end() const;
  };

private:
  std::unique_ptr<SyntaxObject> head_; /* First element (car) */
  std::unique_ptr<SyntaxObject> tail_; /* Second element (cdr) */
};

class SyntaxObject {
  std::variant<Number, Symbol, Cell> obj_;

public:
  SyntaxObject(int64_t num);
  SyntaxObject(const std::string &&sym);
  SyntaxObject(Cell &&cell);

  template <typename T> T *get_if() { return std::get_if<T>(&obj_); }

  template <typename T> T &get() { return std::get<T>(obj_); }

  template <typename T> bool is() const {
    return std::holds_alternative<T>(obj_);
  }
  /*
  bool operator==(const SyntaxObject &other) const {
    return obj_ == other.obj_;
  }
  bool operator!=(const SyntaxObject &other) const { return !(*this == other); }
  */
};

template <Phase P> class Lambda {
public:
  using BodyType =
      std::conditional_t<std::is_same_v<P, Expanded>, FinalExpr, IR1Expr>;

  Lambda(std::vector<Symbol> &&args, BodyType body)
      : args_(std::move(args)), body_(std::move(body)) {}

  std::vector<Symbol> &args() { return args_; }
  BodyType &body() { return body_; }

  llvm::Value *codegen(CodegenContext &context)
    requires(std::is_same_v<P, Expanded>);

private:
  std::vector<Symbol> args_;
  BodyType body_;
};

/*
 * Call - AST node for function calls
 *
 * Represents function calls and closure invocations. Handles both
 * direct function calls (built-ins and compiled functions) and
 * user-defined closures that require environment passing.
 */
template <Phase P> class Call {
public:
  using ExprType =
      std::conditional_t<std::is_same_v<P, Expanded>, FinalExpr, IR1Expr>;

  Call(ExprType callee, std::vector<ExprType> args)
      : callee_(std::move(callee)), args_(std::move(args)) {}

  ExprType &callee() { return callee_; }

  std::vector<ExprType> &args() { return args_; }

  llvm::CallInst *emit(CodegenContext &context)
    requires(std::is_same_v<P, Expanded>);

private:
  ExprType callee_;
  std::vector<ExprType> args_;
};

/*
 * Function - AST node for function definitions
 *
 * Represents user-defined functions that can capture their lexical
 * environment (closures). Generates LLVM functions with environment
 * parameters and handles free variable capture.
 */
template <Phase P> class Def {
public:
  using InitType =
      std::conditional_t<std::is_same_v<P, Expanded>, FinalExpr, IR1Expr>;

  Def(Symbol var, InitType init)
      : var_(std::move(var)), init_(std::move(init)) {}

  Symbol &var() { return var_; }
  InitType &init() { return init_; }

  llvm::GlobalVariable *load(CodegenContext &context)
    requires(std::is_same_v<P, Expanded>);

private:
  Symbol var_;
  InitType init_;
};

/*
 * Setq - AST node for variable assignment
 *
 * Implements variable assignment (setq in Lisp). Handles assignment
 * to variables in different scopes and generates appropriate store
 * instructions.
 */
template <Phase P> class Setq {
public:
  using ValueType =
      std::conditional_t<std::is_same_v<P, Expanded>, FinalExpr, IR1Expr>;

  Setq(Symbol var, ValueType newval)
      : var_(std::move(var)), newval_(std::move(newval)) {}

  Symbol &var() { return var_; }
  ValueType &newval() { return newval_; }

  TaggedLLVMVal codegen(CodegenContext &ctx)
    requires(std::is_same_v<P, Expanded>);

private:
  Symbol var_;
  ValueType newval_;
};

/*
 * If - AST node for conditional expressions
 *
 * Implements if-then-else conditional logic. Generates control flow
 * with basic blocks and PHI nodes to select the appropriate result.
 */
template <Phase P> class If {
public:
  using ExprT =
      std::conditional_t<std::is_same_v<P, Expanded>, FinalExpr, IR1Expr>;

  If(ExprT cond, ExprT thenBranch, ExprT elseBranch)
      : cond_(std::move(cond)), then_(std::move(thenBranch)),
        else_(std::move(elseBranch)) {}

  ExprT &cond() { return cond_; }
  ExprT &thenBranch() { return then_; }
  ExprT &elseBranch() { return else_; }

  llvm::PHINode *codegen(CodegenContext &ctx)
    requires(std::is_same_v<P, Expanded>);

private:
  ExprT cond_;
  ExprT then_;
  ExprT else_;
};

/*
 * Goto - AST node for tagbody construct
 *
 * Implements the Lisp tagbody special form that allows labeled
 * statements and non-local jumps within the body. Creates basic
 * blocks for each tag and preserves expression values.
 */
template <Phase P> class Goto {
public:
  using BodyType =
      std::conditional_t<std::is_same_v<P, Expanded>,
                         std::vector<std::variant<FinalExpr, std::string>>,
                         std::vector<std::variant<IR1Expr, std::string>>>;

  Goto(BodyType &&body) : body_(std::move(body)) {}

  BodyType &body() { return body_; }

  TaggedLLVMVal codegen(CodegenContext &ctx)
    requires(std::is_same_v<P, Expanded>);

private:
  BodyType body_;
};

template <Phase P> class Go {
public:
  Go(std::string tag) : tag_(std::move(tag)) {}

  std::string &tag() { return tag_; }

  void codegen(CodegenContext &ctx)
    requires(std::is_same_v<P, Expanded>);

private:
  std::string tag_;
};

/*
 * Let - AST node for local variable binding
 *
 * Implements local variable binding with initialization (let in Lisp).
 * Creates a new scope with the bound variable available only within
 * the body expression.
 */
template <Phase P> class Let {
public:
  using InitT =
      std::conditional_t<std::is_same_v<P, Expanded>, FinalExpr, IR1Expr>;
  using BodyT = InitT;

  Let(Symbol var, InitT init, BodyT body)
      : var_(std::move(var)), init_(std::move(init)), body_(std::move(body)) {}

  Symbol &var() { return var_; }
  InitT &init() { return init_; }
  BodyT &body() { return body_; }

  TaggedLLVMVal codegen(CodegenContext &ctx)
    requires(std::is_same_v<P, Expanded>);

private:
  Symbol var_;
  InitT init_;
  BodyT body_;
};

TaggedLLVMVal codegen(CodegenContext &CodegenContext, FinalExpr &expr);
