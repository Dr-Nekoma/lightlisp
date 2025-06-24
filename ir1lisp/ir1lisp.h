#include "meta.h"
#include "objects.h"

/*
 * ObjectBuilder - Factory class for transforming Lisp syntax objects into IR1
 * objects
 *
 * This class serves as the primary transformation engine for converting parsed
 * Lisp syntax into the first intermediate representation (IR1) of the compiler
 * pipeline. It uses a registry-based approach where different Lisp forms are
 * mapped to specific build methods for extensible transformation.
 */
struct ObjectBuilder {
  /*
   * BuildMethod - Function pointer type for object construction methods
   *
   * Each build method takes:
   * - ObjectBuilder& : Reference to the builder instance for recursive calls
   * - std::string& name : Name of the Lisp form being processed
   * - SyntaxObject* : Pointer to the syntax object to transform
   *
   * Returns: ObjPtr to the constructed IR1 object
   */
  using BuildMethod = ObjPtr (*)(ObjectBuilder &, std::string &name,
                                 SyntaxObject *);

  /*
   * Constructor - Initializes the object builder
   *
   * Sets up the builders_ registry with mappings from Lisp form names
   * (like "def", "lambda", "if", etc.) to their corresponding
   * transformation functions.
   */
  ObjectBuilder();

  /*
   * build - Main transformation entry point
   *
   * Takes a syntax object and dispatches to the appropriate build method
   * based on the form type. This is the primary interface for converting
   * individual syntax objects to IR1 objects.
   *
   * @param syntax: Reference to the syntax object to transform
   * @return: Smart pointer to the constructed IR1 object
   */
  ObjPtr build(SyntaxObject &syntax);

  /*
   * builders_ - Registry mapping form names to build methods
   *
   * This hash map provides O(1) lookup of the appropriate transformation
   * function for each Lisp form. Keys are form names (strings), values
   * are function pointers implementing the transformation logic.
   */
  std::unordered_map<std::string, BuildMethod> builders_;
};

/*
 * parseArgList - Extract parameter names from argument list syntax
 *
 * Processes a syntax object representing a function parameter list and
 * extracts the individual argument names. Used primarily in function
 * definition processing (lambda, define, etc.).
 *
 * @param syntax: Pointer to syntax object representing the argument list
 * @return: Vector of argument names as strings
 */
std::vector<Symbol> parseArgList(SyntaxObject *syntax);

/*
 * lispListToVec - Convert Lisp list syntax to vector of IR1 objects
 *
 * Recursively transforms a Lisp list syntax object into a vector of
 * IR1 objects. Each element in the list is processed through the
 * ObjectBuilder to create the corresponding IR1 representation.
 *
 * @param builder: Reference to ObjectBuilder for recursive transformation
 * @param syntax: Pointer to syntax object representing a Lisp list
 * @return: Vector of smart pointers to transformed IR1 objects
 */
std::vector<ObjPtr> lispListToVec(ObjectBuilder &builder, SyntaxObject *syntax);

/*
 * codeWalk - Recursive syntax tree traversal and transformation
 *
 * Performs a depth-first traversal of the syntax tree, transforming
 * each node into its corresponding IR1 representation. This function
 * handles the recursive nature of Lisp code structure and delegates
 * to appropriate build methods for each syntax object type.
 *
 * @param builder: Reference to ObjectBuilder instance
 * @param syntax: Reference to syntax object to walk and transform
 * @return: Smart pointer to the transformed IR1 object
 */
ObjPtr codeWalk(ObjectBuilder &builder, SyntaxObject &syntax);

/*
 * ir1LispTransform - Top-level IR1 transformation entry point
 *
 * Main interface function for converting parsed Lisp syntax into IR1.
 * Takes ownership of the input syntax tree and returns the root of
 * the transformed IR1 object tree. This is typically called by the
 * parser after syntax analysis is complete.
 *
 * @param syntax: Unique pointer to root syntax object (transfers ownership)
 * @return: Smart pointer to root IR1 object
 */
ObjPtr ir1LispTransform(std::unique_ptr<SyntaxObject> syntax);