--------------------------------------------------------------------------------
-- | The LLVM abstract syntax.
--

module Llvm.AbsSyn where

import Llvm.MetaData
import Llvm.Types

import Unique

-- | Block labels
type LlvmBlockId = Unique

-- | A block of LLVM code.
data LlvmBlock = LlvmBlock {
    -- | The code label for this block
    blockLabel :: LlvmBlockId,

    -- | A list of LlvmStatement's representing the code for this block.
    -- This list must end with a control flow statement.
    blockStmts :: [LlvmStatement]
  }

type LlvmBlocks = [LlvmBlock]

-- | An LLVM Module. This is a top level container in LLVM.
data LlvmModule = LlvmModule  {
    -- | Comments to include at the start of the module.
    modComments  :: [LMString],

    -- | LLVM Alias type definitions.
    modAliases   :: [LlvmAlias],

    -- | LLVM meta data.
    modMeta      :: [MetaDecl],

    -- | Global variables to include in the module.
    modGlobals   :: [LMGlobal],

    -- | LLVM Functions used in this module but defined in other modules.
    modFwdDecls  :: LlvmFunctionDecls,

    -- | LLVM Functions defined in this module.
    modFuncs     :: LlvmFunctions
  }

-- | An LLVM Function
data LlvmFunction = LlvmFunction {
    -- | The signature of this declared function.
    funcDecl  :: LlvmFunctionDecl,

    -- | The functions arguments
    funcArgs  :: [LMString],

    -- | The function attributes.
    funcAttrs :: [LlvmFuncAttr],

    -- | The section to put the function into,
    funcSect  :: LMSection,

    -- | The body of the functions.
    funcBody  :: LlvmBlocks
  }

type LlvmFunctions = [LlvmFunction]

type SingleThreaded = Bool

-- | LLVM ordering types for synchronization purposes. (Introduced in LLVM
-- 3.0). Please see the LLVM documentation for a better description.
data LlvmSyncOrdering
  -- | Some partial order of operations exists.
  = SyncUnord
  -- | A single total order for operations at a single address exists.
  | SyncMonotonic
  -- | Acquire synchronization operation.
  | SyncAcquire
  -- | Release synchronization operation.
  | SyncRelease
  -- | Acquire + Release synchronization operation.
  | SyncAcqRel
  -- | Full sequential Consistency operation.
  | SyncSeqCst
  deriving (Show, Eq)

-- | Llvm Statements
data LlvmStatement
  {- |
    Assign an expression to an variable:
      * dest:   Variable to assign to
      * source: Source expression
  -}
  = Assignment LlvmVar LlvmExpression

  {- |
    Memory fence operation
  -}
  | Fence Bool LlvmSyncOrdering

  {- |
    Always branch to the target label
  -}
  | Branch LlvmVar

  {- |
    Branch to label targetTrue if cond is true otherwise to label targetFalse
      * cond:        condition that will be tested, must be of type i1
      * targetTrue:  label to branch to if cond is true
      * targetFalse: label to branch to if cond is false
  -}
  | BranchIf LlvmVar LlvmVar LlvmVar

  {- |
    Comment
    Plain comment.
  -}
  | Comment [LMString]

  {- |
    Set a label on this position.
      * name: Identifier of this label, unique for this module
  -}
  | MkLabel LlvmBlockId

  {- |
    Store variable value in pointer ptr. If value is of type t then ptr must
    be of type t*.
      * value: Variable/Constant to store.
      * ptr:   Location to store the value in
  -}
  | Store LlvmVar LlvmVar

  {- |
    Mutliway branch
      * scrutinee: Variable or constant which must be of integer type that is
                   determines which arm is chosen.
      * def:       The default label if there is no match in target.
      * target:    A list of (value,label) where the value is an integer
                   constant and label the corresponding label to jump to if the
                   scrutinee matches the value.
  -}
  | Switch LlvmVar LlvmVar [(LlvmVar, LlvmVar)]

  {- |
    Return a result.
      * result: The variable or constant to return
  -}
  | Return (Maybe LlvmVar)

  {- |
    An instruction for the optimizer that the code following is not reachable
  -}
  | Unreachable

  {- |
    Raise an expression to a statement (if don't want result or want to use
    Llvm unnamed values.
  -}
  | Expr LlvmExpression

  {- |
    A nop LLVM statement. Useful as its often more efficient to use this
    then to wrap LLvmStatement in a Just or [].
  -}
  | Nop

  {- |
    A LLVM statement with metadata attached to it.
  -}
  | MetaStmt [MetaAnnot] LlvmStatement

  deriving (Eq)


-- | Llvm Expressions
data LlvmExpression
  {- |
    Allocate amount * sizeof(tp) bytes on the stack
      * tp:     LlvmType to reserve room for
      * amount: The nr of tp's which must be allocated
  -}
  = Alloca LlvmType Int

  {- |
    Perform the machine operator op on the operands left and right
      * op:    operator
      * left:  left operand
      * right: right operand
  -}
  | LlvmOp LlvmMachOp LlvmVar LlvmVar

  {- |
    Perform a compare operation on the operands left and right
      * op:    operator
      * left:  left operand
      * right: right operand
  -}
  | Compare LlvmCmpOp LlvmVar LlvmVar

  {- |
    Extract a scalar element from a vector
      * val: The vector
      * idx: The index of the scalar within the vector
  -}
  | Extract LlvmVar LlvmVar

  {- |
    Insert a scalar element into a vector
      * val:   The source vector
      * elt:   The scalar to insert
      * index: The index at which to insert the scalar
  -}
  | Insert LlvmVar LlvmVar LlvmVar

  {- |
    Allocate amount * sizeof(tp) bytes on the heap
      * tp:     LlvmType to reserve room for
      * amount: The nr of tp's which must be allocated
  -}
  | Malloc LlvmType Int

  {- |
    Load the value at location ptr
  -}
  | Load LlvmVar

  {- |
    Atomic load of the value at location ptr
  -}
  | ALoad LlvmSyncOrdering SingleThreaded LlvmVar

  {- |
    Navigate in an structure, selecting elements
      * inbound: Is the pointer inbounds? (computed pointer doesn't overflow)
      * ptr:     Location of the structure
      * indexes: A list of indexes to select the correct value.
  -}
  | GetElemPtr Bool LlvmVar [LlvmVar]

  {- |
     Cast the variable from to the to type. This is an abstraction of three
     cast operators in Llvm, inttoptr, prttoint and bitcast.
       * cast: Cast type
       * from: Variable to cast
       * to:   type to cast to
  -}
  | Cast LlvmCastOp LlvmVar LlvmType

  {- |
    Call a function. The result is the value of the expression.
      * tailJumps: CallType to signal if the function should be tail called
      * fnptrval:  An LLVM value containing a pointer to a function to be
                   invoked. Can be indirect. Should be LMFunction type.
      * args:      Concrete arguments for the parameters
      * attrs:     A list of function attributes for the call. Only NoReturn,
                   NoUnwind, ReadOnly and ReadNone are valid here.
  -}
  | Call LlvmCallType LlvmVar [LlvmVar] [LlvmFuncAttr]

  {- |
    Call a function as above but potentially taking metadata as arguments.
      * tailJumps: CallType to signal if the function should be tail called
      * fnptrval:  An LLVM value containing a pointer to a function to be
                   invoked. Can be indirect. Should be LMFunction type.
      * args:      Arguments that may include metadata.
      * attrs:     A list of function attributes for the call. Only NoReturn,
                   NoUnwind, ReadOnly and ReadNone are valid here.
  -}
  | CallM LlvmCallType LlvmVar [MetaExpr] [LlvmFuncAttr]

  {- |
    Merge variables from different basic blocks which are predecessors of this
    basic block in a new variable of type tp.
      * tp:         type of the merged variable, must match the types of the
                    predecessor variables.
      * precessors: A list of variables and the basic block that they originate
                    from.
  -}
  | Phi LlvmType [(LlvmVar,LlvmVar)]

  {- |
    Inline assembly expression. Syntax is very similar to the style used by GCC.
      * assembly:    Actual inline assembly code.
      * constraints: Operand constraints.
      * return ty:   Return type of function.
      * vars:        Any variables involved in the assembly code.
      * sideeffect:  Does the expression have side effects not visible from the
                     constraints list.
      * alignstack:  Should the stack be conservatively aligned before this
                     expression is executed.
  -}
  | Asm LMString LMString LlvmType [LlvmVar] Bool Bool

  {- |
    A LLVM expression with metadata attached to it.
  -}
  | MExpr [MetaAnnot] LlvmExpression

  deriving (Eq)

