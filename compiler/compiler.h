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
  explicit Number(int64_t value);

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
  [[nodiscard]] int64_t getValue() const;

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
  explicit Symbol(std::string name);

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
  [[nodiscard]] const std::string &getName() const;

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

template <> class Lambda<Expanded> : public EmptyBase {
public:
  Lambda(std::vector<Symbol> &&args, ExprPtr<Expanded> body);

  llvm::Value *codegen(CodegenContext &context);

private:
  std::vector<Symbol> args_;
  ExprPtr<Expanded> body_;
};

template <> class Lambda<NotExpanded> : public Macroform {
public:
  Lambda(std::vector<Symbol> &&args, IR1Expr body);

  ExprPtr<Expanded> expand() override;

private:
  std::vector<Symbol> args_;
  IR1Expr body_;
};

/*
 * Call - AST node for function calls
 *
 * Represents function calls and closure invocations. Handles both
 * direct function calls (built-ins and compiled functions) and
 * user-defined closures that require environment passing.
 */
template <> class Call<Expanded> : public EmptyBase {
public:
  /*
   * Construct a Call node for Expanded phase
   *
   * @param callee - Expression that evaluates to the function to call
   * @param args - Vector of argument expressions
   */
  Call(ExprPtr<Expanded> callee, std::vector<ExprPtr<Expanded>> args);

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
   * @return llvm::CallInst* - Function call result as LLVM IR
   * @throws std::runtime_error - If argument count mismatch
   */
  llvm::CallInst *emit(CodegenContext &CodegenContext);

private:
  ExprPtr<Expanded> callee_;
  std::vector<ExprPtr<Expanded>> args_;
};

template <> class Call<NotExpanded> : public Macroform {
public:
  /*
   * Construct a Call node for NotExpanded phase
   *
   * @param callee - IR1Expr that evaluates to the function to call
   * @param args - Vector of IR1Expr argument expressions
   */
  Call(IR1Expr callee, std::vector<IR1Expr> args);

  ExprPtr<Expanded> expand() override;

private:
  IR1Expr callee_;
  std::vector<IR1Expr> args_;
};

/*
 * Function - AST node for function definitions
 *
 * Represents user-defined functions that can capture their lexical
 * environment (closures). Generates LLVM functions with environment
 * parameters and handles free variable capture.
 */
template <> class Def<Expanded> : public EmptyBase {
public:
  /*
   * Construct a Def node for Expanded phase
   *
   * @param var - Name of the variable
   * @param init - Initialization expression
   */
  Def(Symbol var, ExprPtr<Expanded> init);

  /*
   * Def::codegen - Generate LLVM IR for variable definitions
   *
   * Creates a global variable definition:
   * 1. Generates code for initialization expression
   * 2. Creates global variable allocation
   * 3. Registers the variable in global scope
   *
   * @param codegenContext - Provides LLVM context, builder, module, and lexical
   * environment
   * @return llvm::GlobalVariable* - Variable definition as LLVM IR
   */
  llvm::GlobalVariable *load(CodegenContext &CodegenContext);

private:
  Symbol var_;
  ExprPtr<Expanded> init_;
};

template <> class Def<NotExpanded> : public Macroform {
public:
  /*
   * Construct a Def node for NotExpanded phase
   *
   * @param var - Name of the variable
   * @param init - IR1Expr for initialization
   */
  Def(Symbol var, IR1Expr init);

  ExprPtr<Expanded> expand() override;

private:
  Symbol var_;
  IR1Expr init_;
};

/*
 * Setq - AST node for variable assignment
 *
 * Implements variable assignment (setq in Lisp). Handles assignment
 * to variables in different scopes and generates appropriate store
 * instructions.
 */
template <> class Setq<Expanded> : public EmptyBase {
public:
  /*
   * Construct a Setq node for Expanded phase
   *
   * @param var - Name of the variable being assigned
   * @param newval - Expression for the new value
   */
  Setq(Symbol var, ExprPtr<Expanded> newval);

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
   * @return llvm::Value* - Assigned value (right-hand side) as LLVM IR
   * @throws std::runtime_error - If variable not found
   */
  TaggedLLVMVal codegen(CodegenContext &CodegenContext);

private:
  Symbol var_;
  ExprPtr<Expanded> newval_;
};

template <> class Setq<NotExpanded> : public Macroform {
public:
  /*
   * Construct a Setq node for NotExpanded phase
   *
   * @param var - Name of the variable being assigned
   * @param newval - IR1Expr for the new value
   */
  Setq(Symbol var, IR1Expr newval);

  ExprPtr<Expanded> expand() override;

private:
  Symbol var_;
  IR1Expr newval_;
};

/*
 * If - AST node for conditional expressions
 *
 * Implements if-then-else conditional logic. Generates control flow
 * with basic blocks and PHI nodes to select the appropriate result.
 */
template <> class If<Expanded> : public EmptyBase {
public:
  /*
   * Construct an If node for Expanded phase
   *
   * @param cond - Condition expression
   * @param thenBranch - Then branch expression
   * @param elseBranch - Else branch expression
   */
  If(ExprPtr<Expanded> cond, ExprPtr<Expanded> thenBranch,
     ExprPtr<Expanded> elseBranch);

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
   * @return llvm::PHINode* - Result of selected branch as LLVM IR
   */
  llvm::PHINode *codegen(CodegenContext &CodegenContext);

private:
  ExprPtr<Expanded> cond_;
  ExprPtr<Expanded> then_;
  ExprPtr<Expanded> else_;
};

template <> class If<NotExpanded> : public Macroform {
public:
  /*
   * Construct an If node for NotExpanded phase
   *
   * @param cond - Condition IR1Expr
   * @param thenBranch - Then branch IR1Expr
   * @param elseBranch - Else branch IR1Expr
   */
  If(IR1Expr cond, IR1Expr thenBranch, IR1Expr elseBranch);

  ExprPtr<Expanded> expand() override;

private:
  IR1Expr cond_;
  IR1Expr then_;
  IR1Expr else_;
};

/*
 * Goto - AST node for tagbody construct
 *
 * Implements the Lisp tagbody special form that allows labeled
 * statements and non-local jumps within the body. Creates basic
 * blocks for each tag and preserves expression values.
 */

template <> class Goto<Expanded> : public EmptyBase {
public:
  /*
   * Construct a Goto node for Expanded phase
   *
   * @param body - Vector of expressions and string tags that make up the body
   */
  Goto(std::vector<std::variant<ExprPtr<Expanded>, std::string>> &&body);

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
   * @return llvm::Value* - Value of last evaluated expression in body
   */
  TaggedLLVMVal codegen(CodegenContext &CodegenContext);

private:
  std::vector<std::variant<ExprPtr<Expanded>, std::string>> body_;
};

template <> class Goto<NotExpanded> : public Macroform {
public:
  /*
   * Construct a Goto node for NotExpanded phase
   *
   * @param body - Vector of IR1Expr and string tags that make up the body
   */
  Goto(std::vector<std::variant<IR1Expr, std::string>> &&body);

  ExprPtr<Expanded> expand() override;

private:
  std::vector<std::variant<IR1Expr, std::string>> body_;
};

template <> class Go<Expanded> : public EmptyBase {
public:
  Go(std::string &&tag);

  /*
   * Go::codegen - Generate LLVM IR for go expression (jump to tag)
   *
   * Implements jumps to tags defined in enclosing tagbody constructs:
   * 1. Searches for target tag in lexical environment
   * 2. Creates unconditional branch to the tag's basic block
   *
   * @param codegenContext - Provides LLVM context, builder, module, and lexical
   * environment
   * @return void - Control flow never returns
   * @throws std::runtime_error - If target tag not found
   */
  void codegen(CodegenContext &CodegenContext);

private:
  std::string tag_;
};
/*
 * Go - AST node for go expression (jump to tag)
 *
 * Implements jumps to named tags within enclosing tagbody constructs.
 * Generates unconditional branch instructions to the target basic block.
 */
template <> class Go<NotExpanded> : public Macroform {
public:
  Go(std::string &&tag);

  ExprPtr<Expanded> expand() override;

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
template <> class Let<Expanded> : public EmptyBase {
public:
  /*
   * Construct a Let node for Expanded phase
   *
   * @param var - Name of the variable to bind
   * @param init - Expression to initialize the variable
   * @param body - Expression to evaluate with the variable in scope
   */
  Let(Symbol var, ExprPtr<Expanded> init, ExprPtr<Expanded> body);

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
   * @return llvm::Value* - Result of body expression as LLVM IR
   */
  TaggedLLVMVal codegen(CodegenContext &CodegenContext);

private:
  Symbol var_;
  ExprPtr<Expanded> init_;
  ExprPtr<Expanded> body_;
};

template <> class Let<NotExpanded> : public Macroform {
public:
  /*
   * Construct a Let node for NotExpanded phase
   *
   * @param var - Name of the variable to bind
   * @param init - IR1Expr to initialize the variable
   * @param body - IR1Expr to evaluate with the variable in scope
   */
  Let(Symbol var, IR1Expr init, IR1Expr body);

  ExprPtr<Expanded> expand() override;

private:
  Symbol var_;
  IR1Expr init_;
  IR1Expr body_;
};

TaggedLLVMVal codegen(CodegenContext &CodegenContext, ExprPtr<Expanded> &expr);
