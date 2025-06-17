#pragma once
#include <concepts>
#include <memory>
#include <stdexcept>
#include <string>
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

/*
 * SyntaxObject - Variant type for parser result objects
 *
 * Represents the possible results of parsing Lisp syntax,
 * allowing the parser to return different node types without
 * using inheritance and virtual functions.
 */
using SyntaxObject = std::variant<Number, Symbol, Cell>;

/*
 * ObjPtr - Smart pointer for AST nodes
 *
 * Provides ownership semantics for AST nodes with automatic
 * memory management.
 */
using ObjPtr = std::unique_ptr<Object>;

/*
 * IntOpFn - Function type for integer operations
 *
 * Represents functions that operate on integer values and generate
 * LLVM IR for those operations.
 */
using IntOpFn = std::function<llvm::Value *(llvm::IRBuilder<> &, llvm::Value *,
                                            llvm::Value *)>;

/*
 * CodegenContext - Central context for code generation
 *
 * Provides all the necessary components and state for generating
 * LLVM IR from the Lisp AST, including:
 * - LLVM IR generation context
 * - Memory management
 * - Type system
 * - Symbol table for variable resolution
 */
struct CodegenContext {
private:
  // List of constructor functions to be added to the module's global_ctors
  std::vector<llvm::Constant *> initCtors_;

public:
  /*
   * Initialize a new code generation context
   * Sets up the LLVM context, module, and all supporting subsystems
   */
  CodegenContext();

  /*
   * IRGenContext - Core LLVM IR generation components
   *
   * Contains the fundamental LLVM components needed for IR generation:
   * - LLVMContext: Manages LLVM's global data like type uniquing
   * - IRBuilder: Helper for creating LLVM instructions
   * - Module: Container for all generated IR
   */
  struct IRGenContext {
    /*
     * Initialize a new IR generation context
     */
    IRGenContext();

    llvm::LLVMContext context; // LLVM's context for type uniquing and globals
    llvm::IRBuilder<> builder; // Helper for generating LLVM instructions
    llvm::Module module;       // Container for all generated code and data
  };

  /*
   * Memorymanager - Runtime memory allocation system
   *
   * Provides functions and globals for runtime memory management,
   * implementing an allocator for the Lisp runtime.
   */
  class Memorymanager {
  public:
    /*
     * Initialize the memory manager subsystem
     *
     * @param codegenContext - Parent context for LLVM IR generation
     */
    Memorymanager(CodegenContext &codegenContext);

    /*
     * Generate code to set up the memory arena at runtime
     *
     * Creates and initializes the memory arena using mmap
     * at program startup via a constructor function.
     *
     * @param codegenContext - Context for LLVM IR generation
     */
    void prepareArena(CodegenContext &codegenContext);

    /*
     * Generate code to release the memory arena
     *
     * Creates a function to munmap the arena when the program exits.
     *
     * @param irgc - IR generation context containing the module
     */
    void munmapArena(IRGenContext &irgc);

    /*
     * Get the LLVM function pointer for mmap
     *
     * @return Function* - LLVM function for mmap system call
     */
    llvm::Function *getMmapFn();

    /*
     * Get the LLVM function pointer for munmap
     *
     * @return Function* - LLVM function for munmap system call
     */
    llvm::Function *getMunmapFn();

    /*
     * Get the LLVM function pointer for trap (abort)
     *
     * @return Function* - LLVM function for trap/abort
     */
    llvm::Function *getTrapFn();

    /*
     * Get the arena allocator function
     *
     * Returns the function that allocates memory from the arena.
     *
     * @return Function* - Arena allocation function
     */
    llvm::Function *getArenaAllocator();

    /*
     * Get the global variable for the arena pointer
     *
     * @return GlobalVariable* - Pointer to the start of the arena
     */
    llvm::GlobalVariable *getArenaPtrGV();

    /*
     * Get the global variable for the next free arena position
     *
     * @return GlobalVariable* - Next free position in the arena
     */
    llvm::GlobalVariable *getArenaNextGV();

    /*
     * Get the global variable for the arena size
     *
     * @return GlobalVariable* - Total size of the arena
     */
    llvm::GlobalVariable *getArenaSizeGV();

  private:
    /*
     * Define the arena allocation function
     *
     * Creates an LLVM function that implements the arena allocator logic.
     *
     * @param irgc - IR generation context
     * @return Function* - The created arena allocator function
     */
    llvm::Function *defineArenaAlloc(IRGenContext &irgc);

    llvm::Function *mmapFn_;            // Function pointer for mmap
    llvm::Function *munmapFn_;          // Function pointer for munmap
    llvm::GlobalVariable *arenaPtrGV_;  // Global for arena base pointer
    llvm::GlobalVariable *arenaNextGV_; // Global for next free position
    llvm::GlobalVariable *arenaSizeGV_; // Global for arena total size
    llvm::Function *arenaAllocValueFn_; // Function for arena allocation
  };

  /*
   * TypeRegistry - Runtime type system for Lisp values
   *
   * Manages the types used in the Lisp runtime, providing functions
   * for type checking, boxing/unboxing, and type-specific operations.
   */
  class TypeRegistry {
  public:
    /*
     * Initialize the type registry
     *
     * Sets up the LLVM types for Lisp values and creates type descriptors.
     *
     * @param context - Parent code generation context
     */
    TypeRegistry(CodegenContext &context);

    /*
     * BuiltInType - Enumeration of built-in Lisp types
     *
     * Defines the fundamental types supported by the compiler:
     * - Int: Integer values
     * - Cons: Cons cells (pairs)
     * - Fn: Function values (closures)
     * - Null: Null/nil value
     * - T: Boolean true value
     */
    enum class BuiltInType { Int, Cons, Fn, Null, T };

    /*
     * Get the global variable for a type descriptor
     *
     * @param type - Built-in type to get the descriptor for
     * @return GlobalVariable* - Global variable containing the type descriptor
     */
    llvm::GlobalVariable *getType(BuiltInType type);

    /*
     * Create a global variable for a built-in type descriptor
     *
     * @param type - Built-in type to create the descriptor for
     * @return GlobalVariable* - Created global variable
     */
    llvm::GlobalVariable *createBuiltinTypeDescVar(BuiltInType type);

    /*
     * Convert a built-in type to its integer kind value
     *
     * @param type - Built-in type
     * @return int - Integer kind value for the type
     */
    int toKind(BuiltInType type);

    /*
     * Get the string name for a built-in type
     *
     * @param type - Built-in type
     * @return string& - Name of the type
     */
    std::string &toStrName(BuiltInType type);

    /*
     * Get the LLVM type for a built-in type
     *
     * @param type - Built-in type
     * @return Type* - Corresponding LLVM type
     */
    llvm::Type *toLLVMType(BuiltInType type);

    /*
     * Generate code to check if a value has the expected type
     *
     * @param val - Value to check
     * @param type - Expected type
     */
    void emitCheckType(llvm::Value *val, BuiltInType type);

    /*
     * Unbox a value to get its raw payload
     *
     * @param val - Boxed value
     * @param type - Type of the value (for type-specific unpacking)
     * @return Value* - Raw unboxed value
     */
    llvm::Value *unpackVal(llvm::Value *val, BuiltInType type);

    /*
     * Box a raw value with type information
     *
     * @param val - Raw value to box
     * @param type - Type to assign to the boxed value
     * @return Value* - Boxed value
     */
    llvm::Value *packVal(llvm::Value *val, BuiltInType type);

    /*
     * Check a value's type and unbox it in one operation
     *
     * @param val - Boxed value to check and unbox
     * @param type - Expected type
     * @return Value* - Raw unboxed value
     */
    llvm::Value *checkAndUnpack(llvm::Value *val, BuiltInType type);

    /*
     * Get the standard function type for a given arity
     *
     * @param args - Number of arguments the function takes
     * @return FunctionType* - LLVM function type
     */
    llvm::FunctionType *getStdFnType(size_t args);

    /*
     * Create a new empty boxed value
     *
     * @return Value* - Newly allocated boxed value
     */
    llvm::Value *makeBox();

    /*
     * Get the type field from a value
     *
     * @param val - Boxed value
     * @return Value* - Pointer to the type field
     */
    llvm::Value *getValType(llvm::Value *val);

    /*
     * Get the payload field from a value
     *
     * @param val - Boxed value
     * @return Value* - Pointer to the payload field
     */
    llvm::Value *getValPL(llvm::Value *val);

    /*
     * Get the size field from an environment
     *
     * @param env - Environment value
     * @return Value* - Pointer to the size field
     */
    llvm::Value *getEnvSize(llvm::Value *env);

    /*
     * Get the storage field from an environment
     *
     * @param env - Environment value
     * @return Value* - Pointer to the storage field
     */
    llvm::Value *getEnvStorage(llvm::Value *env);

    /*
     * Get the environment field from a closure
     *
     * @param closure - Closure value
     * @return Value* - Pointer to the environment field
     */
    llvm::Value *getClosureEnv(llvm::Value *closure);

    /*
     * Get the function field from a closure
     *
     * @param closure - Closure value
     * @return Value* - Pointer to the function field
     */
    llvm::Value *getClosureFn(llvm::Value *closure);

    /*
     * Get the size field from a closure
     *
     * @param closure - Closure value
     * @return Value* - Pointer to the size field
     */
    llvm::Value *getClosureSize(llvm::Value *closure);

    /*
     * Load the type from a value
     *
     * @param val - Boxed value
     * @return Value* - Loaded type value
     */
    llvm::Value *loadValType(llvm::Value *val);

    /*
     * Load the size from an environment
     *
     * @param env - Environment value
     * @return Value* - Loaded size value
     */
    llvm::Value *loadEnvSize(llvm::Value *env);

    /*
     * Load the storage pointer from an environment
     *
     * @param env - Environment value
     * @return Value* - Loaded storage pointer
     */
    llvm::Value *loadEnvStorage(llvm::Value *env);

    /*
     * Load the environment from a closure
     *
     * @param closure - Closure value
     * @return Value* - Loaded environment value
     */
    llvm::Value *loadClosureEnv(llvm::Value *closure);

    /*
     * Load the function from a closure
     *
     * @param closure - Closure value
     * @return Value* - Loaded function value
     */
    llvm::Value *loadClosureFn(llvm::Value *closure);

    /*
     * Load the size from a closure
     *
     * @param closure - Closure value
     * @return Value* - Loaded size value
     */
    llvm::Value *loadClosureSize(llvm::Value *closure);

    /*
     * Store a type into a value
     *
     * @param type - Type to store
     * @param val - Value to store into
     * @return Value* - Stored instruction
     */
    llvm::Value *storeValType(llvm::Value *type, llvm::Value *val);

    /*
     * Store a payload into a value
     *
     * @param payload - Payload to store
     * @param val - Value to store into
     * @return Value* - Stored instruction
     */
    llvm::Value *storeValPL(llvm::Value *payload, llvm::Value *val);

    /*
     * Store a size into an environment
     *
     * @param size - Size to store
     * @param env - Environment to store into
     * @return Value* - Stored instruction
     */
    llvm::Value *storeEnvSize(llvm::Value *size, llvm::Value *env);

    /*
     * Store a storage pointer into an environment
     *
     * @param storage - Storage pointer to store
     * @param env - Environment to store into
     * @return Value* - Stored instruction
     */
    llvm::Value *storeEnvStorage(llvm::Value *storage, llvm::Value *env);

    /*
     * Store an environment into a closure
     *
     * @param env - Environment to store
     * @param closure - Closure to store into
     * @return Value* - Stored instruction
     */
    llvm::Value *storeClosureEnv(llvm::Value *env, llvm::Value *closure);

    /*
     * Store a function into a closure
     *
     * @param fn - Function to store
     * @param closure - Closure to store into
     * @return Value* - Stored instruction
     */
    llvm::Value *storeClosureFn(llvm::Function *fn, llvm::Value *closure);

    /*
     * Store a size into a closure
     *
     * @param size - Size to store
     * @param closure - Closure to store into
     * @return Value* - Stored instruction
     */
    llvm::Value *storeClosureSize(llvm::Value *size, llvm::Value *closure);

    /*
     * Copy one value into another
     *
     * @param src - Source value
     * @param dest - Destination value
     * @return Value* - Last store instruction
     */
    llvm::Value *copyValInto(llvm::Value *src, llvm::Value *dest);

    /*
     * Generate code to check if a value is truthy
     *
     * @param what - Value to check for truthiness
     * @return Value* - Boolean result of the truth check
     */
    llvm::Value *emitTrueCheck(llvm::Value *what);

    /*
     * Generate code to check if a value has a specific type
     *
     * @param what - Value to check
     * @param type - Type to check against
     * @return Value* - Boolean result of the type check
     */
    llvm::Value *emitTypeCheck(llvm::Value *what, BuiltInType type);

    // Common LLVM types used throughout the compiler
    llvm::PointerType *ptrType;     // Generic pointer type (i8*)
    llvm::IntegerType *i32Type;     // 32-bit integer type
    llvm::IntegerType *i64Type;     // 64-bit integer type
    llvm::StructType *typeDescType; // Type descriptor structure
    llvm::StructType *valueType;    // Boxed value structure
    llvm::StructType *consType;     // Cons cell structure
    llvm::StructType *envType;      // Environment structure
    llvm::StructType *closureType;  // Closure structure

  private:
    CodegenContext *parent_; // Parent context

    /*
     * Create the LLVM structure type for boxed values
     *
     * @param irgc - IR generation context
     * @return StructType* - Created value type
     */
    llvm::StructType *makeValueType(IRGenContext &irgc);

    // Map of built-in types to their global descriptor variables
    std::unordered_map<BuiltInType, llvm::GlobalVariable *> builtInTypes_;
  };

  /*
   * SymbolTable - Variable and function name resolution
   *
   * Manages variable scopes, built-in functions, and symbol resolution
   * for the compiler.
   */
  class SymbolTable {
  public:
    /*
     * Initialize a new symbol table
     *
     * Sets up built-in functions and global runtime functions.
     *
     * @param codegenContext - Parent code generation context
     */
    SymbolTable(CodegenContext &codegenContext);

    /*
     * Enter a new lexical scope
     *
     * Creates a new scope for local variables, optionally with a
     * closure environment.
     *
     * @param newEnv - Optional environment pointer for closures
     */
    void enterScope(llvm::Argument *newEnv = nullptr);

    /*
     * Exit the current lexical scope
     *
     * Removes the innermost scope, potentially recording free variables
     * for closures.
     *
     * @param isFnScope - Whether this is a function scope (affects closure
     * handling)
     */
    void exitScope(bool isFnScope);

    /*
     * Check if we're at the top-level scope
     *
     * @return bool - True if in the top-level scope
     */
    bool isTopLevel();

    /*
     * Look up a built-in function by name
     *
     * @param name - Function name to look up
     * @return Function* - LLVM function pointer or nullptr if not found
     */
    llvm::Function *getBuiltInFn(const std::string &name);

    /*
     * Add a local variable to the current scope
     *
     * @param name - Variable name
     * @param inst - LLVM allocation instruction for the variable
     */
    void addVar(const std::string &name, llvm::AllocaInst *inst);

    /*
     * Add a global variable
     *
     * @param name - Variable name
     * @param inst - LLVM global variable
     */
    void addVar(const std::string &name, llvm::GlobalVariable *inst);

    /*
     * Record a free variable captured by a closure
     *
     * @param name - Variable name
     * @param var - LLVM allocation instruction for the variable
     */
    void addFreeVar(const std::string &name, llvm::AllocaInst *var);

    /*
     * Get the list of free variables for the current closure
     *
     * @return vector<pair<string, AllocaInst*>> - Free variables with names
     */
    std::vector<std::pair<std::string, llvm::AllocaInst *>> popFreeVars();

    /*
     * VarStatus - Status of a variable lookup
     *
     * Indicates where a variable was found in the scope hierarchy:
     * - Local: In the current scope
     * - Captured: In an outer scope, needs to be captured
     * - Global: In the global scope
     * - NotFound: Variable not defined
     */
    enum class VarStatus { Local, Captured, Global, GlobalConstant, NotFound };

    /*
     * Look up a variable by name
     *
     * @param name - Variable name to look up
     * @return pair<VarInst, VarStatus> - Variable instance and status
     */
    std::pair<VarInst, VarStatus> lookUpVar(const std::string &name);

    llvm::GlobalVariable *getConstGlobal(const std::string &name);

    /*
     * Get the number of free variables in the current closure
     *
     * @return size_t - Number of free variables
     */
    size_t freeVarsSize();

    /*
     * Get the current environment argument
     *
     * @return Argument* - Current environment argument or nullptr
     */
    llvm::Argument *getCurrentEnv();

    /*
     * Get the tag environments stack for go/tagbody
     *
     * @return vector<unordered_map<string, BasicBlock*>>& - Tag environments
     */
    std::vector<std::unordered_map<std::string, llvm::BasicBlock *>> &tagEnvs();

    /*
     * Get the innermost tag environment
     *
     * @return unordered_map<string, BasicBlock*>& - Tag environment
     */
    std::unordered_map<std::string, llvm::BasicBlock *> &lastTagEnv();

    /*
     * Get the trap (abort) function
     *
     * @return Function* - LLVM function for trap/abort
     */
    llvm::Function *getTrapFn();

    /*
     * Get the constructor block for top-level initialization
     *
     * @return BasicBlock* - LLVM basic block for initialization code
     */
    llvm::BasicBlock *getCtorBlock();

    /*
     * Get the constructor function for top-level initialization
     *
     * @return Function* - LLVM function for initialization
     */
    llvm::Function *getCtorFn();

    /*
     * Get the print function for debugging
     *
     * @return Function* - LLVM function for printing
     */
    llvm::Function *getPrintFn();

    /*
     * Register a closure constructor
     *
     * Creates a wrapper for a function pointer to be used as a closure.
     *
     * @param fnPtr - Function pointer for the closure body
     * @return Value* - Generated closure wrapper value
     */
    llvm::Value *constructClosureWrapper(llvm::Function *fnPtr);

    /*
     * Set parameters for function wrapper construction
     *
     * Configures the free variables and argument count for the current
     * function wrapper being constructed.
     *
     * @param vars - Vector of free variable allocations
     * @param size - Number of arguments the function takes
     */
    void setFnWrapperParameters(std::vector<llvm::AllocaInst *> &&vars,
                                size_t size);

    /*
     * Set the current insertion block for IR generation
     *
     * Changes the current basic block where new instructions will be inserted.
     *
     * @param block - Basic block to set as current
     * @param descend - Whether to push this block onto the block stack
     */
    void setInsertBlock(llvm::BasicBlock *block, bool descend);

    /*
     * Get the current insertion block
     *
     * @return BasicBlock* - Current basic block for IR insertion
     */
    llvm::BasicBlock *getCurrentBlock();

    /*
     * Pop the current insertion block from the stack
     *
     * Returns to the previous insertion block, used for nested block
     * management.
     */
    void ascend();

  private:
    /*
     * Set up internal runtime functions
     *
     * Initializes core runtime functions needed by the compiler including:
     * - Memory management functions (trap/abort)
     * - Debugging functions (print)
     * - Type system support functions
     * These are essential functions that the generated code relies on.
     */
    void setUpInternalFns();

    /*
     * Set up standard library functions
     *
     * Registers built-in Lisp functions and operators including:
     * - Arithmetic operations (+, -, *, /, etc.)
     * - Comparison operations (=, <, >, etc.)
     * - List operations (car, cdr, cons, etc.)
     * - Type predicates and (later) conversion functions
     */
    void setUpSTDLib();

    /*
     * EnvManager - Manages lexical environments for variable lookup
     *
     * Tracks variable scopes and function boundaries for proper
     * variable resolution and closure creation.
     */
    struct EnvManager {
      EnvManager() = default;

      // Stack of scopes, each scope is a map of names to allocations
      std::vector<std::unordered_map<std::string, llvm::AllocaInst *>>
          env_stack;

      // Indices of function scopes in the env_stack
      std::vector<size_t> fn_idxes;
    };

    llvm::Function *trapFn_;  // Trap/abort function
    llvm::Function *printFn_; // Print function for debugging

    // Global constants (e.g., nil, t)
    std::unordered_map<std::string, llvm::GlobalVariable *> constantGlobals_;

    // Built-in functions (primitive operations)
    std::unordered_map<std::string, llvm::Function *> builtInFns_;

    EnvManager named_values_; // Variable scope manager

    // Global variables
    std::unordered_map<std::string, llvm::GlobalVariable *> globals_;

    // Tag environments for go/tagbody
    std::vector<std::unordered_map<std::string, llvm::BasicBlock *>> tagEnvs_;

    // Stack of free variables for each closure level
    std::vector<std::vector<std::pair<std::string, llvm::AllocaInst *>>>
        freeVars_;

    // Stack of environment arguments for nested closures
    std::vector<llvm::Argument *> envStack_;

    llvm::BasicBlock *ctorBlock_; // Block for initialization code
    std::vector<llvm::BasicBlock *> insertBlocks_;
    llvm::Function *ctorFn_; // Function for initialization

    std::optional<std::vector<llvm::AllocaInst *>> curFreeVars_;
    std::optional<size_t> curArgSize_;

    CodegenContext *parent_; // Parent context
  };

  IRGenContext context;         // Core LLVM context, builder, module
  TypeRegistry type_manager;    // Type system management
  Memorymanager memory_manager; // Runtime memory management
  SymbolTable lexenv;           // Symbol and variable management

  /*
   * Add a constructor function to the module
   *
   * @param priority - Priority value (lower runs earlier)
   * @param ctor - Constructor function to add
   */
  void addCtor(size_t priority, llvm::Function *ctor);

  /*
   * Emit the constructor function for creating closures
   *
   * Generates code to create all closures at program startup.
   */
  void emitClosuresCtor();
};

/*
 * createClosurecall - Generate a call to a closure
 *
 * Creates the LLVM IR for calling a user-defined closure,
 * including passing the environment and arguments.
 *
 * @param codegenContext - Code generation context
 * @param inst - Closure value to call
 * @param args - Arguments to the call
 * @return Value* - Result of the call
 */
llvm::Value *createClosurecall(CodegenContext &codegenContext,
                               llvm::Value *inst, std::vector<ObjPtr> &args);

/*
 * Type namespace - Type constants for cleaner code
 *
 * Provides aliases for the built-in types to avoid using the
 * full enum class syntax throughout the code.
 */
namespace Type {
const auto Int = CodegenContext::TypeRegistry::BuiltInType::Int;
const auto Cons = CodegenContext::TypeRegistry::BuiltInType::Cons;
const auto Fn = CodegenContext::TypeRegistry::BuiltInType::Fn;
const auto Null = CodegenContext::TypeRegistry::BuiltInType::Null;
const auto T = CodegenContext::TypeRegistry::BuiltInType::T;
} // namespace Type