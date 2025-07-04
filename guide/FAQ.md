Here's a list of basic things you will need to know and keep in memory at all times.

--------------------------------------------------------------------------------

# LLVMContext

- **What it is**: a container for all the uniqued LLVM internal objects--types, constants, metadata, and so on.
- **When to use**: you normally create one per "compilation session." Pass it into every `Module`, `IRBuilder`, and type/construction call.

```c++
auto ctx = std::make_unique<llvm::LLVMContext>();
```

--------------------------------------------------------------------------------

# Module

- **Definition**: a translation unit--holds a collection of functions, global variables, and symbol/table information.
- **Key methods**:

  - `Module(const std::string &name, LLVMContext &C)` – create a new module.
  - `getOrInsertFunction(name, FunctionType*)` – declare or fetch a function prototype.
  - `getFunction(name)` – fetch an existing `Function*` or `nullptr`.
  - `getNamedGlobal(name)` – fetch a `GlobalVariable*`.

- **Usage**: everything you emit (functions, globals) goes into one `Module`.

```c++
llvm::Module M("my_module", *ctx);
```

--------------------------------------------------------------------------------

# IRBuilder<>

- **Definition**: a helper that remembers an "insertion point" and provides `CreateXYZ()` methods for every IR instruction.
- **Construction**:

  ```c++
  llvm::IRBuilder<> builder(ctx.get());
  ```

- **Key methods**:

  - `SetInsertPoint(BasicBlock *BB)` – begin inserting at the end of `BB`.
  - `CreateAdd(lhs, rhs, name)` – emit an `add` instruction, returns an `llvm::Value*`.
  - `CreateCall(fn, args, name)` – emit a call instruction.
  - `CreateStructGEP(StructType*, ptr, idx, name)` – emit a _getelementptr_ into a struct field.
  - `CreateLoad(type, ptr, name)` – emit a `load`.
  - `CreateStore(value, ptr)` – emit a `store`.
  - `CreateBr(destBB)` / `CreateCondBr(cond, thenBB, elseBB)` – emit (conditional) branches.
  - `CreateRet(value)` – emit a return.

--------------------------------------------------------------------------------

# BasicBlock

- **Definition**: a sequence of instructions ending in a _terminator_ (branch or return).
- **Creation**:

  ```c++
  auto BB = llvm::BasicBlock::Create(ctx, "entry", parentFunction);
  builder.SetInsertPoint(BB);
  ```

- **Notes**: you must always terminate a block (e.g. `CreateBr`, `CreateRet`); otherwise verification fails.

--------------------------------------------------------------------------------

# Function

- **Definition**: a top-level IR function.
- **Creation**:

  ```c++
  autofType = llvm::FunctionType::get(retTy, argTypes, /*vararg=*/false);
  auto F  = llvm::Function::Create(
     fType, llvm::Function::ExternalLinkage, "foo", &module);
  ```

- **Arguments**: `for (auto &arg : F->args()) arg.setName("x");`

- **Blocks**: once created, you can insert `BasicBlock`s into it.

--------------------------------------------------------------------------------

# Calls

- **`CreateCall`**

  ```c++
  // direct call: Func is a Function* or FunctionCallee
  builder.CreateCall(Func, { arg0, arg1, … }, "calltmp");
  ```

  - Creates a **CallInst** in IR.
  - Returns a `Value*` corresponding to the call's return value (or `void` if none).
  - You can mark it as a tail call:

    ```c++
    auto CI = builder.CreateCall(fn, args);
    CI->setTailCallKind(llvm::CallInst::TCK_Tail);
    ```

- **CallInst**

  - An instruction subclass representing a function invocation.
  - Internally holds a pointer to the callee, argument list, and attributes (calling convention, tail‐call hints, etc.).

--------------------------------------------------------------------------------

## `CreateRet`

Covered above under **Return**, but to reiterate:

```c++
builder.CreateRet(val);     // for non‐void
builder.CreateRetVoid();    // for void
```

This emits the final instruction of a BasicBlock and ends it. You cannot append any more instructions after a terminator.

--------------------------------------------------------------------------------

# Types

- **IntegerType**: `Type::getInt32Ty(ctx)`, `getInt64Ty(ctx)`
- **PointerType**: `PointerType::get(elementTy, addressSpace)`
- **StructType**:

  ```c++
  auto ST = StructType::create(ctx, "MyStruct");
  ST->setBody({ Ty1, Ty2, … }, /*isPacked=*/false);
  ```

- **FunctionType**: `FunctionType::get(retTy, {argTys…}, vararg)`

- **ArrayType**: `ArrayType::get(elemTy, numElems)`

- **Notes**: All type objects are uniqued in the `LLVMContext`.

--------------------------------------------------------------------------------

# Values & Constants

- **Value**: abstract base for _everything_ you can produce.
- **Constants** (subclass of `Value`):

  - `ConstantInt::get(i32Ty, 42)` → an `i32 42`.
  - `ConstantFP::get(doubleTy, 3.14)` → a float constant.
  - `ConstantDataArray::getString(ctx, "foo")` → a `[4 x i8] c"foo\00"`.
  - `ConstantExpr::getInBoundsGetElementPtr(...)` → a compile-time GEP.

- **GlobalVariable**: a global constant or mutable data.

--------------------------------------------------------------------------------

# CreateLoad vs CreateStore

- **`CreateLoad(Type *ty, Value *ptr, name)`**

  - Loads from memory at address `ptr`. `ty` tells LLVM "I expect `ptr` to be a `ty*`."
  - Returns a new `Value*` of type `ty`.

- **`CreateStore(Value *val, Value *ptr)`**

  - Stores `val` into memory at `ptr`. `val->getType()` must match the pointer's element type.

--------------------------------------------------------------------------------

# GetElementPtr (`CreateGEP` / `CreateStructGEP`)

- **Purpose**: compute addresses within aggregates (arrays or structs).
- **`CreateStructGEP(StructType *STy, Value *ptr, unsigned idx, name)`**

  - Treats `ptr` as pointing to `STy`, and computes the address of field `idx`.

- **`getInBoundsGetElementPtr(Type *elementTy, Constant *C, ArrayRef<Constant*> idxs)`**

  - Constant version: navigates `idxs` starting from `C`. `{0, i}` selects element `i`.

- **Example**:

  ```llvm
  ; %ptr : %MyStruct*
  %fieldPtr = getelementptr inbounds %MyStruct, %MyStruct* %ptr, i32 0, i32 2
  ```

  picks the 3rd field.

--------------------------------------------------------------------------------

# Control-Flow & Terminators

LLVM requires every **BasicBlock** to end in a **terminator** instruction that transfers control somewhere else. The common ones are:

- **Unconditional branch**

  ```c++
  // jumps to destBB, no return value
  builder.CreateBr(destBB);
  ```

  Emits:

  ```llvm
  br label %destBB
  ```

- **Conditional branch**

  ```c++
  // condBool is an i1 (boolean), thenBB/elseBB are BasicBlock*
  builder.CreateCondBr(condBool, thenBB, elseBB);
  ```

  Emits:

  ```llvm
  br i1 %condBool, label %thenBB, label %elseBB
  ```

- **Return**

  ```c++
  // return a value of the function’s return type
  builder.CreateRet(value);
  // or for a void function:
  builder.CreateRetVoid();
  ```

  Emits:

  ```llvm
  ret i32 %value        ; or `ret void`
  ```

- **Switch**

  ```c++
  // value : i32, defaultBB, list of (caseValue, caseBB)
  auto sw = builder.CreateSwitch(value, defaultBB, numCases);
  sw->addCase(builder.getInt32(42), caseBB42);
  ```

  Emits:

  ```llvm
  switch i32 %value, label %defaultBB [
    i32 42, label %caseBB42
    … 
  ]
  ```

- **Indirect branch**

  ```c++
  // addr : label*, defaultBB
  auto ib = builder.CreateIndirectBr(addr, numDests);
  ib->addDestination(destBB1);
  ```

  Emits:

  ```llvm
  indirectbr label* %addr, [ label %dest1, label %dest2, … ]
  ```

## Terminator Instructions & `getTerminator()`

- **Terminator** = any instruction that ends a BasicBlock (`ret`, `br`, `switch`, ...).
- **`curBB->getTerminator()`**

  - Returns the current terminator instruction of `curBB`, or `nullptr` if none exists yet.
  - Useful to check "has this block already been closed?" before inserting a new branch.

--------------------------------------------------------------------------------

# `CreateEntryBlockAlloca`

Often you need a stack slot for a local variable or to hold a function argument. It's best practice to create `alloca`s in the **entry block** so that LLVM's optimization passes can more easily promote them to registers.

A typical helper:

```c++
llvm::AllocaInst *CreateEntryBlockAlloca(
    CodegenContext &ctx,
    llvm::Function    *F,
    llvm::Type        *ty,
    const std::string &name) {
  llvm::IRBuilder<> TmpB(&F->getEntryBlock(),
                        F->getEntryBlock().begin());
  return TmpB.CreateAlloca(ty, /*ArraySize=*/nullptr, name);
}
```

- **`alloca`**

  ```llvm
  %slot = alloca i64, align 8
  ```

  Allocates `sizeof(i64)` bytes on the stack frame of the current function. Returns a pointer (`i64*`) that you can `store` into and `load` from.

--------------------------------------------------------------------------------

# PHI Nodes

Used to **merge SSA values** from multiple predecessor blocks:

```c++
// inside the merge block, after creating ThenBB and ElseBB
auto PN = builder.CreatePHI(resultTy, /*numIncoming=*/2, "iftmp");
PN->addIncoming(thenValue, ThenBB);
PN->addIncoming(elseValue, ElseBB);
```

Emits:

```llvm
%iftmp = phi i64 [ %thenValue, %ThenBB ], [ %elseValue, %ElseBB ]
```

- **PhiNode**

  - A special IR instruction that selects a value based on which control‐flow edge was taken.
  - Must appear as the **first** non‐PHI instruction in a BasicBlock.

--------------------------------------------------------------------------------

# Putting it together

Every time you build control flow:

1. **Create new BasicBlocks** for each branch case.
2. **Emit your body** into each block (using `builder.SetInsertPoint`).
3. **At the end** of each block, check `if (!block->getTerminator())` then emit a `CreateBr` or `CreateRet`.
4. **In merge points**, use `CreatePHI` to unify values.

With calls, allocas, loads, stores, branches, and PHIs, you've got the full toolkit to emit well-formed SSA IR that LLVM can optimize and lower to machine code.
