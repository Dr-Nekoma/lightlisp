#pragma once
#include "meta.h"
#include "util.h"

/*
 * Object - Base class for all AST nodes
 *
 * Abstract base class that defines the interface for all nodes in the
 * abstract syntax tree. Each node must be able to generate LLVM IR
 * for itself through the codegen method.
 */
class Object {
public:
  virtual ~Object() {}

  /*
   * Generate LLVM IR for this AST node
   *
   * Pure virtual function that must be implemented by all AST node types.
   * Transforms the high-level AST representation into LLVM intermediate
   * representation.
   *
   * @param CodegenContext - Context providing LLVM infrastructure and state
   * @return TaggedLLVMVal - Generated LLVM value for this node
   */
  virtual TaggedLLVMVal codegen(CodegenContext &CodegenContext) = 0;
};

/*
 * Number - AST node for integer literals
 *
 * Represents literal integer values in the Lisp source code.
 * Stores the numeric value and generates LLVM IR for boxed integers.
 */
class Number : public Object {
public:
  /*
   * Construct a Number node with the given integer value
   *
   * @param value - Integer value (defaults to 0)
   */
  explicit Number(int64_t value = 0) : value_(value) {}

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
  TaggedLLVMVal codegen(CodegenContext &CodegenContext) override;

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
 * to Variable nodes during semantic analysis for variable references
 * or used directly for special forms and function names.
 */
class Symbol {
public:
  /*
   * Construct a Symbol with the given name
   *
   * @param value - String name of the symbol
   */
  explicit Symbol(std::string value) : value_(std::move(value)) {}

  /*
   * Get the symbol's name
   *
   * @return const string& - Reference to the symbol name
   */
  const std::string &getName() { return value_; }

private:
  std::string value_; /* The symbol's string name */
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
  Cell() : head_(nullptr), tail_(nullptr) {}

  /*
   * Construct a cell with head and tail elements
   *
   * @param head - First element of the pair (car)
   * @param tail - Second element of the pair (cdr)
   */
  Cell(std::unique_ptr<SyntaxObject> head, std::unique_ptr<SyntaxObject> tail)
      : head_(std::move(head)), tail_(std::move(tail)) {}

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
    explicit ListIterator(SyntaxObject *node) : node_(node) {}

    /*
     * Dereference operator - get the head of current cell
     *
     * @return reference - Reference to the head element
     */
    reference operator*() const {
      auto &ret = std::get<Cell>(*node_);
      return *ret.head_;
    }

    /*
     * Arrow operator for member access
     *
     * @return pointer - Pointer to current node
     */
    pointer operator->() const { return std::addressof(*node_); }

    /*
     * Get the current cell pointer
     *
     * @return pointer - Pointer to current node
     */
    pointer getCell() { return node_; }

    /*
     * Equality comparison
     *
     * @param o - Other iterator to compare with
     * @return bool - True if iterators point to same node
     */
    bool operator==(ListIterator const &o) const { return node_ == o.node_; }

    /*
     * Inequality comparison
     *
     * @param o - Other iterator to compare with
     * @return bool - True if iterators point to different nodes
     */
    bool operator!=(ListIterator const &o) const { return !(*this == o); }

    /*
     * Pre-increment operator - advance to next cell
     *
     * Follows the cdr pointer to the next cell in the list.
     *
     * @return ListIterator& - Reference to this iterator
     * @throws std::runtime_error - If incrementing past end or improper list
     */
    ListIterator &operator++() {
      if (!node_)
        throw std::runtime_error("increment past end");
      auto c = std::get_if<Cell>(node_);
      if (!c)
        throw std::runtime_error("not a proper list");
      node_ = c->get<1>().get(); /* Follow cdr pointer */
      if (node_ && !std::holds_alternative<Cell>(*node_))
        throw std::runtime_error("not a proper list");
      return *this;
    }

    /*
     * Post-increment operator
     *
     * @return ListIterator - Copy of iterator before increment
     */
    ListIterator operator++(int) {
      auto tmp = *this;
      ++*this;
      return tmp;
    }

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
    ListView(SyntaxObject *head, SyntaxObject *tail)
        : head_(head), tail_(tail) {}

    /*
     * Construct a view with just the head (tail defaults to nullptr)
     *
     * @param head - Start of the list
     */
    ListView(SyntaxObject *head) : head_(head), tail_(nullptr) {}

    SyntaxObject *head_; /* Start of the list */
    SyntaxObject *tail_; /* End of the list */

    /*
     * Get iterator to the beginning of the list
     *
     * @return ListIterator - Iterator pointing to first element
     */
    [[nodiscard]] ListIterator begin() const { return ListIterator(head_); }

    /*
     * Get iterator to the end of the list
     *
     * @return ListIterator - Iterator pointing past last element
     */
    [[nodiscard]] ListIterator end() const { return ListIterator(tail_); }
  };

private:
  std::unique_ptr<SyntaxObject> head_; /* First element (car) */
  std::unique_ptr<SyntaxObject> tail_; /* Second element (cdr) */
};

/*
 * Variable - AST node for variable references
 *
 * Represents references to variables in the Lisp code. Handles
 * variable lookup through different scopes (local, captured, global)
 * and generates appropriate load instructions.
 */
class Variable : public Object {
public:
  /*
   * Construct a Variable node with the given name
   *
   * @param name - Name of the variable to reference
   */
  Variable(std::string name) : name_(std::move(name)) {}

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
  TaggedLLVMVal codegen(CodegenContext &CodegenContext) override;

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
 * Call - AST node for function calls
 *
 * Represents function calls and closure invocations. Handles both
 * direct function calls (built-ins and compiled functions) and
 * user-defined closures that require environment passing.
 */
class Call : public Object {
public:
  /*
   * Construct a Call node
   *
   * @param callee - Expression that evaluates to the function to call
   * @param args - Vector of argument expressions
   */
  Call(ObjPtr callee, std::vector<ObjPtr> args)
      : callee_(std::move(callee)), args_(std::move(args)) {}

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
  TaggedLLVMVal codegen(CodegenContext &CodegenContext) override;

private:
  ObjPtr callee_;            // Function expression to call
  std::vector<ObjPtr> args_; // Argument expressions
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

/*
 * Function - AST node for function definitions
 *
 * Represents user-defined functions that can capture their lexical
 * environment (closures). Generates LLVM functions with environment
 * parameters and handles free variable capture.
 */
class Def : public Object {
public:
  /*
   * Construct a Function node
   *
   * @param name - Name of the function
   * @param args - Parameter names
   * @param body - Body expression of the function
   */
  Def(std::unique_ptr<Variable> var, ObjPtr init)
      : var_(std::move(var)), init_(std::move(init)) {}

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
  TaggedLLVMVal codegen(CodegenContext &CodegenContext) override;

private:
  std::unique_ptr<Variable> var_; // Variable node for LHS
  ObjPtr init_;                   // Initialization expression
};

/*
 * Setq - AST node for variable assignment
 *
 * Implements variable assignment (setq in Lisp). Handles assignment
 * to variables in different scopes and generates appropriate store
 * instructions.
 */
class Setq : public Object {
public:
  /*
   * Construct a Setq node
   *
   * @param name - Name of the variable being assigned
   * @param fst - Variable node for the left-hand side
   * @param snd - Expression for the right-hand side value
   */
  Setq(std::unique_ptr<Variable> var, ObjPtr newval)
      : var_(std::move(var)), newval_(std::move(newval)) {}

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
  TaggedLLVMVal codegen(CodegenContext &CodegenContext) override;

private:
  std::unique_ptr<Variable> var_; // Variable node for LHS
  ObjPtr newval_;                 // Value expression for RHS
};

class Lambda : public Object {
public:
  Lambda(std::vector<std::string> &&args, ObjPtr body)
      : args_(std::move(args)), body_(std::move(body)) {}

  TaggedLLVMVal codegen(CodegenContext &CodegenContext) override;

private:
  std::vector<std::string> args_; // Parameter names
  ObjPtr body_;                   // Function body expression
};
/*
 * If - AST node for conditional expressions
 *
 * Implements if-then-else conditional logic. Generates control flow
 * with basic blocks and PHI nodes to select the appropriate result.
 */
class If : public Object {
public:
  /*
   * Construct an If node
   *
   * @param Cond - Condition expression
   * @param Then - Expression to evaluate if condition is true
   * @param Else - Expression to evaluate if condition is false
   */
  If(ObjPtr cond, ObjPtr thenBranch, ObjPtr elseBranch)
      : cond_(std::move(cond)), then_(std::move(thenBranch)),
        else_(std::move(elseBranch)) {}

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
  TaggedLLVMVal codegen(CodegenContext &CodegenContext) override;

private:
  ObjPtr cond_; // Condition expression
  ObjPtr then_; // Then-branch expression
  ObjPtr else_; // Else-branch expression
};

/*
 * Goto - AST node for tagbody construct
 *
 * Implements the Lisp tagbody special form that allows labeled
 * statements and non-local jumps within the body. Creates basic
 * blocks for each tag and preserves expression values.
 */
class Goto : public Object {
public:
  /*
   * Construct a Goto (tagbody) node
   *
   * @param body - Vector of expressions and string tags that make up the body
   */
  Goto(std::vector<std::variant<ObjPtr, std::string>> &&body)
      : body_(std::move(body)) {}

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
  TaggedLLVMVal codegen(CodegenContext &CodegenContext) override;

private:
  /* Body containing both expressions and string tags for jumps */
  std::vector<std::variant<ObjPtr, std::string>> body_;
};

/*
 * Go - AST node for go expression (jump to tag)
 *
 * Implements jumps to named tags within enclosing tagbody constructs.
 * Generates unconditional branch instructions to the target basic block.
 */
class Go : public Object {
public:
  Go(std::string &&tag) : tag_(tag) {}

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
  TaggedLLVMVal codegen(CodegenContext &CodegenContext) override;

private:
  std::string tag_; // Name of the tag to jump to
};

/*
 * Let - AST node for local variable binding
 *
 * Implements local variable binding with initialization (let in Lisp).
 * Creates a new scope with the bound variable available only within
 * the body expression.
 */
class Let : public Object {
public:
  /*
   * Construct a Let node
   *
   * @param name - Name of the variable to bind
   * @param init - Expression to initialize the variable
   * @param body - Expression to evaluate with the variable in scope
   */
  Let(std::unique_ptr<Variable> var, ObjPtr init, ObjPtr body)
      : var_(std::move(var)), init_(std::move(init)), body_(std::move(body)) {}

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
  TaggedLLVMVal codegen(CodegenContext &CodegenContext) override;

private:
  std::unique_ptr<Variable> var_; // Variable node for LHS
  ObjPtr init_;                   // Initialization expression
  ObjPtr body_;                   // Body expression with variable in scope
};
