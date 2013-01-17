--------------------------------------------------------------------------------
-- | The LLVM Type System.
--

module Llvm.Types where

#include "HsVersions.h"

import Data.Char
import Data.Int
import Data.List (intercalate)
import Numeric

import DynFlags
import FastString
import Unique

-- from NCG
import PprBase

import GHC.Float

-- -----------------------------------------------------------------------------
-- * LLVM Basic Types and Variables
--

-- | A global mutable variable. Maybe defined or external
type LMGlobal = (LlvmVar, Maybe LlvmStatic)
-- | A String in LLVM
type LMString = FastString

-- | A type alias
type LlvmAlias = (LMString, LlvmType)

-- | Llvm Types
data LlvmType
  = LMInt Int            -- ^ An integer with a given width in bits.
  | LMFloat              -- ^ 32 bit floating point
  | LMDouble             -- ^ 64 bit floating point
  | LMFloat80            -- ^ 80 bit (x86 only) floating point
  | LMFloat128           -- ^ 128 bit floating point
  | LMPointer LlvmType   -- ^ A pointer to a 'LlvmType'
  | LMArray Int LlvmType -- ^ An array of 'LlvmType'
  | LMLabel              -- ^ A 'LlvmVar' can represent a label (address)
  | LMVoid               -- ^ Void type
  | LMStruct [LlvmType]  -- ^ Structure type
  | LMAlias LlvmAlias    -- ^ A type alias

  -- | Function type, used to create pointers to functions
  | LMFunction LlvmFunctionDecl
  deriving (Eq)

instance Show LlvmType where
  show (LMInt size    ) = "i" ++ show size
  show (LMFloat       ) = "float"
  show (LMDouble      ) = "double"
  show (LMFloat80     ) = "x86_fp80"
  show (LMFloat128    ) = "fp128"
  show (LMPointer x   ) = show x ++ "*"
  show (LMArray nr tp ) = "[" ++ show nr ++ " x " ++ show tp ++ "]"
  show (LMLabel       ) = "label"
  show (LMVoid        ) = "void"
  show (LMStruct tys  ) = "<{" ++ (commaCat tys) ++ "}>"

  show (LMFunction (LlvmFunctionDecl _ _ _ r varg p _))
    = let varg' = case varg of
                        VarArgs | null args -> "..."
                                | otherwise -> ", ..."
                        _otherwise          -> ""
          -- by default we don't print param attributes
          args = intercalate ", " $ map (show . fst) p
      in show r ++ " (" ++ args ++ varg' ++ ")"

  show (LMAlias (s,_)) = "%" ++ unpackFS s

-- | LLVM metadata values. Used for representing debug and optimization
-- information.
data LlvmMetaVal
  -- | Metadata string
  = MetaStr LMString
  -- | Metadata node
  | MetaNode LlvmMetaUnamed
  -- | Normal value type as metadata
  | MetaVar LlvmVar
  deriving (Eq)

-- | LLVM metadata nodes.
data LlvmMeta
  -- | Unamed metadata
  = MetaUnamed LlvmMetaUnamed [LlvmMetaVal]
  -- | Named metadata
  | MetaNamed LMString [LlvmMetaUnamed]
  deriving (Eq)

-- | Unamed metadata variable.
newtype LlvmMetaUnamed = LMMetaUnamed Int

instance Eq LlvmMetaUnamed where
  (==) (LMMetaUnamed n) (LMMetaUnamed m) = n == m

instance Show LlvmMetaVal where
  show (MetaStr  s) = "metadata !\"" ++ unpackFS s ++ "\""
  show (MetaNode n) = "metadata " ++ show n
  show (MetaVar  v) = show v

instance Show LlvmMetaUnamed where
  show (LMMetaUnamed u) = "!" ++ show u

instance Show LlvmMeta where
  show (MetaUnamed m _) = show m
  show (MetaNamed  m _) = "!" ++ unpackFS m

-- | An LLVM section definition. If Nothing then let LLVM decide the section
type LMSection = Maybe LMString
type LMAlign = Maybe Int
type LMConst = Bool -- ^ is a variable constant or not

-- | LLVM Variables
data LlvmVar
  -- | Variables with a global scope.
  = LMGlobalVar LMString LlvmType LlvmLinkageType LMSection LMAlign LMConst
  -- | Variables local to a function or parameters.
  | LMLocalVar Unique LlvmType
  -- | Named local variables. Sometimes we need to be able to explicitly name
  -- variables (e.g for function arguments).
  | LMNLocalVar LMString LlvmType
  -- | A constant variable
  | LMLitVar LlvmLit
  deriving (Eq)

instance Show LlvmVar where
  show (LMLitVar x) = show x
  show (x         ) = show (getVarType x) ++ " " ++ getName x


-- | Llvm Literal Data.
--
-- These can be used inline in expressions.
data LlvmLit
  -- | Refers to an integer constant (i64 42).
  = LMIntLit Integer LlvmType
  -- | Floating point literal
  | LMFloatLit Double LlvmType
  -- | Literal NULL, only applicable to pointer types
  | LMNullLit LlvmType
  -- | Undefined value, random bit pattern. Useful for optimisations.
  | LMUndefLit LlvmType
  deriving (Eq)

instance Show LlvmLit where
  show l = show (getLitType l) ++ " " ++ getLit l


-- | Llvm Static Data.
--
-- These represent the possible global level variables and constants.
data LlvmStatic
  = LMComment LMString                  -- ^ A comment in a static section
  | LMStaticLit LlvmLit                 -- ^ A static variant of a literal value
  | LMUninitType LlvmType               -- ^ For uninitialised data
  | LMStaticStr LMString LlvmType       -- ^ Defines a static 'LMString'
  | LMStaticArray [LlvmStatic] LlvmType -- ^ A static array
  | LMStaticStruc [LlvmStatic] LlvmType -- ^ A static structure type
  | LMStaticPointer LlvmVar             -- ^ A pointer to other data

  -- static expressions, could split out but leave
  -- for moment for ease of use. Not many of them.

  | LMBitc LlvmStatic LlvmType         -- ^ Pointer to Pointer conversion
  | LMPtoI LlvmStatic LlvmType         -- ^ Pointer to Integer conversion
  | LMAdd LlvmStatic LlvmStatic        -- ^ Constant addition operation
  | LMSub LlvmStatic LlvmStatic        -- ^ Constant subtraction operation

instance Show LlvmStatic where
  show (LMComment       s) = "; " ++ unpackFS s
  show (LMStaticLit   l  ) = show l
  show (LMUninitType    t) = show t ++ " undef"
  show (LMStaticStr   s t) = show t ++ " c\"" ++ unpackFS s ++ "\\00\""
  show (LMStaticArray d t) = show t ++ " [" ++ commaCat d ++ "]"
  show (LMStaticStruc d t) = show t ++ "<{" ++ commaCat d ++ "}>"
  show (LMStaticPointer v) = show v
  show (LMBitc v t)
      = show t ++ " bitcast (" ++ show v ++ " to " ++ show t ++ ")"
  show (LMPtoI v t)
      = show t ++ " ptrtoint (" ++ show v ++ " to " ++ show t ++ ")"
  show (LMAdd s1 s2)
      = let ty1 = getStatType s1
            op  = if isFloat ty1 then " fadd (" else " add ("
        in if ty1 == getStatType s2
                then show ty1 ++ op ++ show s1 ++ "," ++ show s2 ++ ")"
                else error $ "LMAdd with different types! s1: "
                        ++ show s1 ++ ", s2: " ++ show s2
  show (LMSub s1 s2)
      = let ty1 = getStatType s1
            op  = if isFloat ty1 then " fsub (" else " sub ("
        in if ty1 == getStatType s2
                then show ty1 ++ op ++ show s1 ++ "," ++ show s2 ++ ")"
                else error $ "LMSub with different types! s1: "
                        ++ show s1 ++ ", s2: " ++ show s2


-- | Concatenate an array together, separated by commas
commaCat :: Show a => [a] -> String
commaCat xs = intercalate ", " $ map show xs

-- -----------------------------------------------------------------------------
-- ** Operations on LLVM Basic Types and Variables
--

-- | Return the variable name or value of the 'LlvmVar'
-- in Llvm IR textual representation (e.g. @\@x@, @%y@ or @42@).
getName :: LlvmVar -> String
getName v@(LMGlobalVar _ _ _ _ _ _) = "@" ++ getPlainName v
getName v@(LMLocalVar  _ _        ) = "%" ++ getPlainName v
getName v@(LMNLocalVar _ _        ) = "%" ++ getPlainName v
getName v@(LMLitVar    _          ) = getPlainName v

-- | Return the variable name or value of the 'LlvmVar'
-- in a plain textual representation (e.g. @x@, @y@ or @42@).
getPlainName :: LlvmVar -> String
getPlainName (LMGlobalVar x _ _ _ _ _) = unpackFS x
getPlainName (LMLocalVar  x LMLabel  ) = show x
getPlainName (LMLocalVar  x _        ) = "l" ++ show x
getPlainName (LMNLocalVar x _        ) = unpackFS x
getPlainName (LMLitVar    x          ) = getLit x

-- | Print a literal value. No type.
getLit :: LlvmLit -> String
getLit (LMIntLit i (LMInt 32)) = show (fromInteger i :: Int32)
getLit (LMIntLit i (LMInt 64)) = show (fromInteger i :: Int64)
getLit (LMIntLit i _         ) = show (fromInteger i :: Int)
-- See Note [LLVM Float Types].
getLit (LMFloatLit r LMFloat ) = (dToStr . widenFp . narrowFp) r
getLit (LMFloatLit r LMDouble) = dToStr r
getLit f@(LMFloatLit _ _) = error $ "Can't print this float literal!" ++ show f
getLit (LMNullLit _     ) = "null"
getLit (LMUndefLit _    ) = "undef"

-- | Return the 'LlvmType' of the 'LlvmVar'
getVarType :: LlvmVar -> LlvmType
getVarType (LMGlobalVar _ y _ _ _ _) = y
getVarType (LMLocalVar  _ y        ) = y
getVarType (LMNLocalVar _ y        ) = y
getVarType (LMLitVar    l          ) = getLitType l

-- | Return the 'LlvmType' of a 'LlvmLit'
getLitType :: LlvmLit -> LlvmType
getLitType (LMIntLit   _ t) = t
getLitType (LMFloatLit _ t) = t
getLitType (LMNullLit    t) = t
getLitType (LMUndefLit   t) = t

-- | Return the 'LlvmType' of the 'LlvmStatic'
getStatType :: LlvmStatic -> LlvmType
getStatType (LMStaticLit   l  ) = getLitType l
getStatType (LMUninitType    t) = t
getStatType (LMStaticStr   _ t) = t
getStatType (LMStaticArray _ t) = t
getStatType (LMStaticStruc _ t) = t
getStatType (LMStaticPointer v) = getVarType v
getStatType (LMBitc        _ t) = t
getStatType (LMPtoI        _ t) = t
getStatType (LMAdd         t _) = getStatType t
getStatType (LMSub         t _) = getStatType t
getStatType (LMComment       _) = error "Can't call getStatType on LMComment!"

-- | Return the 'LlvmType' of the 'LMGlobal'
getGlobalType :: LMGlobal -> LlvmType
getGlobalType (v, _) = getVarType v

-- | Return the 'LlvmVar' part of a 'LMGlobal'
getGlobalVar :: LMGlobal -> LlvmVar
getGlobalVar (v, _) = v

-- | Return the 'LlvmLinkageType' for a 'LlvmVar'
getLink :: LlvmVar -> LlvmLinkageType
getLink (LMGlobalVar _ _ l _ _ _) = l
getLink _                         = Internal

-- | Add a pointer indirection to the supplied type. 'LMLabel' and 'LMVoid'
-- cannot be lifted.
pLift :: LlvmType -> LlvmType
pLift (LMLabel) = error "Labels are unliftable"
pLift (LMVoid)  = error "Voids are unliftable"
pLift x         = LMPointer x

-- | Lower a variable of 'LMPointer' type.
pVarLift :: LlvmVar -> LlvmVar
pVarLift (LMGlobalVar s t l x a c) = LMGlobalVar s (pLift t) l x a c
pVarLift (LMLocalVar  s t        ) = LMLocalVar  s (pLift t)
pVarLift (LMNLocalVar s t        ) = LMNLocalVar s (pLift t)
pVarLift (LMLitVar    _          ) = error $ "Can't lower a literal type!"

-- | Remove the pointer indirection of the supplied type. Only 'LMPointer'
-- constructors can be lowered.
pLower :: LlvmType -> LlvmType
pLower (LMPointer x) = x
pLower x  = error $ show x ++ " is a unlowerable type, need a pointer"

-- | Lower a variable of 'LMPointer' type.
pVarLower :: LlvmVar -> LlvmVar
pVarLower (LMGlobalVar s t l x a c) = LMGlobalVar s (pLower t) l x a c
pVarLower (LMLocalVar  s t        ) = LMLocalVar  s (pLower t)
pVarLower (LMNLocalVar s t        ) = LMNLocalVar s (pLower t)
pVarLower (LMLitVar    _          ) = error $ "Can't lower a literal type!"

-- | Test if the given 'LlvmType' is an integer
isInt :: LlvmType -> Bool
isInt (LMInt _) = True
isInt _         = False

-- | Test if the given 'LlvmType' is a floating point type
isFloat :: LlvmType -> Bool
isFloat LMFloat    = True
isFloat LMDouble   = True
isFloat LMFloat80  = True
isFloat LMFloat128 = True
isFloat _          = False

-- | Test if the given 'LlvmType' is an 'LMPointer' construct
isPointer :: LlvmType -> Bool
isPointer (LMPointer _) = True
isPointer _             = False

-- | Test if a 'LlvmVar' is global.
isGlobal :: LlvmVar -> Bool
isGlobal (LMGlobalVar _ _ _ _ _ _) = True
isGlobal _                         = False

-- | Width in bits of an 'LlvmType', returns 0 if not applicable
llvmWidthInBits :: DynFlags -> LlvmType -> Int
llvmWidthInBits _      (LMInt n)       = n
llvmWidthInBits _      (LMFloat)       = 32
llvmWidthInBits _      (LMDouble)      = 64
llvmWidthInBits _      (LMFloat80)     = 80
llvmWidthInBits _      (LMFloat128)    = 128
-- Could return either a pointer width here or the width of what
-- it points to. We will go with the former for now.
llvmWidthInBits dflags (LMPointer _)   = llvmWidthInBits dflags (llvmWord dflags)
llvmWidthInBits dflags (LMArray _ _)   = llvmWidthInBits dflags (llvmWord dflags)
llvmWidthInBits _      LMLabel         = 0
llvmWidthInBits _      LMVoid          = 0
llvmWidthInBits dflags (LMStruct tys)  = sum $ map (llvmWidthInBits dflags) tys
llvmWidthInBits _      (LMFunction  _) = 0
llvmWidthInBits dflags (LMAlias (_,t)) = llvmWidthInBits dflags t


-- -----------------------------------------------------------------------------
-- ** Shortcut for Common Types
--

i128, i64, i32, i16, i8, i1, i8Ptr :: LlvmType
i128  = LMInt 128
i64   = LMInt  64
i32   = LMInt  32
i16   = LMInt  16
i8    = LMInt   8
i1    = LMInt   1
i8Ptr = pLift i8

-- | The target architectures word size
llvmWord, llvmWordPtr :: DynFlags -> LlvmType
llvmWord    dflags = LMInt (wORD_SIZE dflags * 8)
llvmWordPtr dflags = pLift (llvmWord dflags)

-- -----------------------------------------------------------------------------
-- * LLVM Function Types
--

-- | An LLVM Function
data LlvmFunctionDecl = LlvmFunctionDecl {
        -- | Unique identifier of the function
        decName       :: LMString,
        -- | LinkageType of the function
        funcLinkage   :: LlvmLinkageType,
        -- | The calling convention of the function
        funcCc        :: LlvmCallConvention,
        -- | Type of the returned value
        decReturnType :: LlvmType,
        -- | Indicates if this function uses varargs
        decVarargs    :: LlvmParameterListType,
        -- | Parameter types and attributes
        decParams     :: [LlvmParameter],
        -- | Function align value, must be power of 2
        funcAlign     :: LMAlign
  }
  deriving (Eq)

instance Show LlvmFunctionDecl where
  show (LlvmFunctionDecl n l c r varg p a)
    = let varg' = case varg of
                        VarArgs | null args -> "..."
                                | otherwise -> ", ..."
                        _otherwise          -> ""
          align = case a of
                       Just a' -> " align " ++ show a'
                       Nothing -> ""
          -- by default we don't print param attributes
          args = intercalate ", " $ map (show . fst) p
      in show l ++ " " ++ show c ++ " " ++ show r ++ " @" ++ unpackFS n ++
             "(" ++ args ++ varg' ++ ")" ++ align

type LlvmFunctionDecls = [LlvmFunctionDecl]

type LlvmParameter = (LlvmType, [LlvmParamAttr])

-- | LLVM Parameter Attributes.
--
-- Parameter attributes are used to communicate additional information about
-- the result or parameters of a function
data LlvmParamAttr
  -- | This indicates to the code generator that the parameter or return value
  -- should be zero-extended to a 32-bit value by the caller (for a parameter)
  -- or the callee (for a return value).
  = ZeroExt
  -- | This indicates to the code generator that the parameter or return value
  -- should be sign-extended to a 32-bit value by the caller (for a parameter)
  -- or the callee (for a return value).
  | SignExt
  -- | This indicates that this parameter or return value should be treated in
  -- a special target-dependent fashion during while emitting code for a
  -- function call or return (usually, by putting it in a register as opposed
  -- to memory).
  | InReg
  -- | This indicates that the pointer parameter should really be passed by
  -- value to the function.
  | ByVal
  -- | This indicates that the pointer parameter specifies the address of a
  -- structure that is the return value of the function in the source program.
  | SRet
  -- | This indicates that the pointer does not alias any global or any other
  -- parameter.
  | NoAlias
  -- | This indicates that the callee does not make any copies of the pointer
  -- that outlive the callee itself
  | NoCapture
  -- | This indicates that the pointer parameter can be excised using the
  -- trampoline intrinsics.
  | Nest
  deriving (Eq)

instance Show LlvmParamAttr where
  show ZeroExt   = "zeroext"
  show SignExt   = "signext"
  show InReg     = "inreg"
  show ByVal     = "byval"
  show SRet      = "sret"
  show NoAlias   = "noalias"
  show NoCapture = "nocapture"
  show Nest      = "nest"

-- | Llvm Function Attributes.
--
-- Function attributes are set to communicate additional information about a
-- function. Function attributes are considered to be part of the function,
-- not of the function type, so functions with different parameter attributes
-- can have the same function type. Functions can have multiple attributes.
--
-- Descriptions taken from <http://llvm.org/docs/LangRef.html#fnattrs>
data LlvmFuncAttr
  -- | This attribute indicates that the inliner should attempt to inline this
  -- function into callers whenever possible, ignoring any active inlining
  -- size threshold for this caller.
  = AlwaysInline
  -- | This attribute indicates that the source code contained a hint that
  -- inlining this function is desirable (such as the \"inline\" keyword in
  -- C/C++). It is just a hint; it imposes no requirements on the inliner.
  | InlineHint
  -- | This attribute indicates that the inliner should never inline this
  -- function in any situation. This attribute may not be used together
  -- with the alwaysinline attribute.
  | NoInline
  -- | This attribute suggests that optimization passes and code generator
  -- passes make choices that keep the code size of this function low, and
  -- otherwise do optimizations specifically to reduce code size.
  | OptSize
  -- | This function attribute indicates that the function never returns
  -- normally. This produces undefined behavior at runtime if the function
  -- ever does dynamically return.
  | NoReturn
  -- | This function attribute indicates that the function never returns with
  -- an unwind or exceptional control flow. If the function does unwind, its
  -- runtime behavior is undefined.
  | NoUnwind
  -- | This attribute indicates that the function computes its result (or
  -- decides to unwind an exception) based strictly on its arguments, without
  -- dereferencing any pointer arguments or otherwise accessing any mutable
  -- state (e.g. memory, control registers, etc) visible to caller functions.
  -- It does not write through any pointer arguments (including byval
  -- arguments) and never changes any state visible to callers. This means
  -- that it cannot unwind exceptions by calling the C++ exception throwing
  -- methods, but could use the unwind instruction.
  | ReadNone
  -- | This attribute indicates that the function does not write through any
  -- pointer arguments (including byval arguments) or otherwise modify any
  -- state (e.g. memory, control registers, etc) visible to caller functions.
  -- It may dereference pointer arguments and read state that may be set in
  -- the caller. A readonly function always returns the same value (or unwinds
  -- an exception identically) when called with the same set of arguments and
  -- global state. It cannot unwind an exception by calling the C++ exception
  -- throwing methods, but may use the unwind instruction.
  | ReadOnly
  -- | This attribute indicates that the function should emit a stack smashing
  -- protector. It is in the form of a \"canary\"—a random value placed on the
  -- stack before the local variables that's checked upon return from the
  -- function to see if it has been overwritten. A heuristic is used to
  -- determine if a function needs stack protectors or not.
  --
  -- If a function that has an ssp attribute is inlined into a function that
  -- doesn't have an ssp attribute, then the resulting function will have an
  -- ssp attribute.
  | Ssp
  -- | This attribute indicates that the function should always emit a stack
  -- smashing protector. This overrides the ssp function attribute.
  --
  -- If a function that has an sspreq attribute is inlined into a function
  -- that doesn't have an sspreq attribute or which has an ssp attribute,
  -- then the resulting function will have an sspreq attribute.
  | SspReq
  -- | This attribute indicates that the code generator should not use a red
  -- zone, even if the target-specific ABI normally permits it.
  | NoRedZone
  -- | This attributes disables implicit floating point instructions.
  | NoImplicitFloat
  -- | This attribute disables prologue / epilogue emission for the function.
  -- This can have very system-specific consequences.
  | Naked
  deriving (Eq)

instance Show LlvmFuncAttr where
  show AlwaysInline       = "alwaysinline"
  show InlineHint         = "inlinehint"
  show NoInline           = "noinline"
  show OptSize            = "optsize"
  show NoReturn           = "noreturn"
  show NoUnwind           = "nounwind"
  show ReadNone           = "readnon"
  show ReadOnly           = "readonly"
  show Ssp                = "ssp"
  show SspReq             = "ssqreq"
  show NoRedZone          = "noredzone"
  show NoImplicitFloat    = "noimplicitfloat"
  show Naked              = "naked"


-- | Different types to call a function.
data LlvmCallType
  -- | Normal call, allocate a new stack frame.
  = StdCall
  -- | Tail call, perform the call in the current stack frame.
  | TailCall
  deriving (Eq,Show)

-- | Different calling conventions a function can use.
data LlvmCallConvention
  -- | The C calling convention.
  -- This calling convention (the default if no other calling convention is
  -- specified) matches the target C calling conventions. This calling
  -- convention supports varargs function calls and tolerates some mismatch in
  -- the declared prototype and implemented declaration of the function (as
  -- does normal C).
  = CC_Ccc
  -- | This calling convention attempts to make calls as fast as possible
  -- (e.g. by passing things in registers). This calling convention allows
  -- the target to use whatever tricks it wants to produce fast code for the
  -- target, without having to conform to an externally specified ABI
  -- (Application Binary Interface). Implementations of this convention should
  -- allow arbitrary tail call optimization to be supported. This calling
  -- convention does not support varargs and requires the prototype of al
  -- callees to exactly match the prototype of the function definition.
  | CC_Fastcc
  -- | This calling convention attempts to make code in the caller as efficient
  -- as possible under the assumption that the call is not commonly executed.
  -- As such, these calls often preserve all registers so that the call does
  -- not break any live ranges in the caller side. This calling convention
  -- does not support varargs and requires the prototype of all callees to
  -- exactly match the prototype of the function definition.
  | CC_Coldcc
  -- | Any calling convention may be specified by number, allowing
  -- target-specific calling conventions to be used. Target specific calling
  -- conventions start at 64.
  | CC_Ncc Int
  -- | X86 Specific 'StdCall' convention. LLVM includes a specific alias for it
  -- rather than just using CC_Ncc.
  | CC_X86_Stdcc
  deriving (Eq)

instance Show LlvmCallConvention where
  show CC_Ccc       = "ccc"
  show CC_Fastcc    = "fastcc"
  show CC_Coldcc    = "coldcc"
  show (CC_Ncc i)   = "cc " ++ show i
  show CC_X86_Stdcc = "x86_stdcallcc"


-- | Functions can have a fixed amount of parameters, or a variable amount.
data LlvmParameterListType
  -- Fixed amount of arguments.
  = FixedArgs
  -- Variable amount of arguments.
  | VarArgs
  deriving (Eq,Show)


-- | Linkage type of a symbol.
--
-- The description of the constructors is copied from the Llvm Assembly Language
-- Reference Manual <http://www.llvm.org/docs/LangRef.html#linkage>, because
-- they correspond to the Llvm linkage types.
data LlvmLinkageType
  -- | Global values with internal linkage are only directly accessible by
  -- objects in the current module. In particular, linking code into a module
  -- with an internal global value may cause the internal to be renamed as
  -- necessary to avoid collisions. Because the symbol is internal to the
  -- module, all references can be updated. This corresponds to the notion
  -- of the @static@ keyword in C.
  = Internal
  -- | Globals with @linkonce@ linkage are merged with other globals of the
  -- same name when linkage occurs. This is typically used to implement
  -- inline functions, templates, or other code which must be generated
  -- in each translation unit that uses it. Unreferenced linkonce globals are
  -- allowed to be discarded.
  | LinkOnce
  -- | @weak@ linkage is exactly the same as linkonce linkage, except that
  -- unreferenced weak globals may not be discarded. This is used for globals
  -- that may be emitted in multiple translation units, but that are not
  -- guaranteed to be emitted into every translation unit that uses them. One
  -- example of this are common globals in C, such as @int X;@ at global
  -- scope.
  | Weak
  -- | @appending@ linkage may only be applied to global variables of pointer
  -- to array type. When two global variables with appending linkage are
  -- linked together, the two global arrays are appended together. This is
  -- the Llvm, typesafe, equivalent of having the system linker append
  -- together @sections@ with identical names when .o files are linked.
  | Appending
  -- | The semantics of this linkage follow the ELF model: the symbol is weak
  -- until linked, if not linked, the symbol becomes null instead of being an
  -- undefined reference.
  | ExternWeak
  -- | The symbol participates in linkage and can be used to resolve external
  --  symbol references.
  | ExternallyVisible
  -- | Alias for 'ExternallyVisible' but with explicit textual form in LLVM
  --  assembly.
  | External
  deriving (Eq)

instance Show LlvmLinkageType where
  show Internal          = "internal"
  show LinkOnce          = "linkonce"
  show Weak              = "weak"
  show Appending         = "appending"
  show ExternWeak        = "extern_weak"
  -- ExternallyVisible does not have a textual representation, it is
  -- the linkage type a function resolves to if no other is specified
  -- in Llvm.
  show ExternallyVisible = ""
  show External          = "external"


-- -----------------------------------------------------------------------------
-- * LLVM Operations
--

-- | Llvm binary operators machine operations.
data LlvmMachOp
  = LM_MO_Add  -- ^ add two integer, floating point or vector values.
  | LM_MO_Sub  -- ^ subtract two ...
  | LM_MO_Mul  -- ^ multiply ..
  | LM_MO_UDiv -- ^ unsigned integer or vector division.
  | LM_MO_SDiv -- ^ signed integer ..
  | LM_MO_URem -- ^ unsigned integer or vector remainder (mod)
  | LM_MO_SRem -- ^ signed ...

  | LM_MO_FAdd -- ^ add two floating point or vector values.
  | LM_MO_FSub -- ^ subtract two ...
  | LM_MO_FMul -- ^ multiply ...
  | LM_MO_FDiv -- ^ divide ...
  | LM_MO_FRem -- ^ remainder ...

  -- | Left shift
  | LM_MO_Shl
  -- | Logical shift right
  -- Shift right, filling with zero
  | LM_MO_LShr
  -- | Arithmetic shift right
  -- The most significant bits of the result will be equal to the sign bit of
  -- the left operand.
  | LM_MO_AShr

  | LM_MO_And -- ^ AND bitwise logical operation.
  | LM_MO_Or  -- ^ OR bitwise logical operation.
  | LM_MO_Xor -- ^ XOR bitwise logical operation.
  deriving (Eq)

instance Show LlvmMachOp where
  show LM_MO_Add  = "add"
  show LM_MO_Sub  = "sub"
  show LM_MO_Mul  = "mul"
  show LM_MO_UDiv = "udiv"
  show LM_MO_SDiv = "sdiv"
  show LM_MO_URem = "urem"
  show LM_MO_SRem = "srem"
  show LM_MO_FAdd = "fadd"
  show LM_MO_FSub = "fsub"
  show LM_MO_FMul = "fmul"
  show LM_MO_FDiv = "fdiv"
  show LM_MO_FRem = "frem"
  show LM_MO_Shl  = "shl"
  show LM_MO_LShr = "lshr"
  show LM_MO_AShr = "ashr"
  show LM_MO_And  = "and"
  show LM_MO_Or   = "or"
  show LM_MO_Xor  = "xor"


-- | Llvm compare operations.
data LlvmCmpOp
  = LM_CMP_Eq  -- ^ Equal (Signed and Unsigned)
  | LM_CMP_Ne  -- ^ Not equal (Signed and Unsigned)
  | LM_CMP_Ugt -- ^ Unsigned greater than
  | LM_CMP_Uge -- ^ Unsigned greater than or equal
  | LM_CMP_Ult -- ^ Unsigned less than
  | LM_CMP_Ule -- ^ Unsigned less than or equal
  | LM_CMP_Sgt -- ^ Signed greater than
  | LM_CMP_Sge -- ^ Signed greater than or equal
  | LM_CMP_Slt -- ^ Signed less than
  | LM_CMP_Sle -- ^ Signed less than or equal

  -- Float comparisons. GHC uses a mix of ordered and unordered float
  -- comparisons.
  | LM_CMP_Feq -- ^ Float equal
  | LM_CMP_Fne -- ^ Float not equal
  | LM_CMP_Fgt -- ^ Float greater than
  | LM_CMP_Fge -- ^ Float greater than or equal
  | LM_CMP_Flt -- ^ Float less than
  | LM_CMP_Fle -- ^ Float less than or equal
  deriving (Eq)

instance Show LlvmCmpOp where
  show LM_CMP_Eq  = "eq"
  show LM_CMP_Ne  = "ne"
  show LM_CMP_Ugt = "ugt"
  show LM_CMP_Uge = "uge"
  show LM_CMP_Ult = "ult"
  show LM_CMP_Ule = "ule"
  show LM_CMP_Sgt = "sgt"
  show LM_CMP_Sge = "sge"
  show LM_CMP_Slt = "slt"
  show LM_CMP_Sle = "sle"
  show LM_CMP_Feq = "oeq"
  show LM_CMP_Fne = "une"
  show LM_CMP_Fgt = "ogt"
  show LM_CMP_Fge = "oge"
  show LM_CMP_Flt = "olt"
  show LM_CMP_Fle = "ole"


-- | Llvm cast operations.
data LlvmCastOp
  = LM_Trunc    -- ^ Integer truncate
  | LM_Zext     -- ^ Integer extend (zero fill)
  | LM_Sext     -- ^ Integer extend (sign fill)
  | LM_Fptrunc  -- ^ Float truncate
  | LM_Fpext    -- ^ Float extend
  | LM_Fptoui   -- ^ Float to unsigned Integer
  | LM_Fptosi   -- ^ Float to signed Integer
  | LM_Uitofp   -- ^ Unsigned Integer to Float
  | LM_Sitofp   -- ^ Signed Int to Float
  | LM_Ptrtoint -- ^ Pointer to Integer
  | LM_Inttoptr -- ^ Integer to Pointer
  | LM_Bitcast  -- ^ Cast between types where no bit manipulation is needed
  deriving (Eq)

instance Show LlvmCastOp where
  show LM_Trunc    = "trunc"
  show LM_Zext     = "zext"
  show LM_Sext     = "sext"
  show LM_Fptrunc  = "fptrunc"
  show LM_Fpext    = "fpext"
  show LM_Fptoui   = "fptoui"
  show LM_Fptosi   = "fptosi"
  show LM_Uitofp   = "uitofp"
  show LM_Sitofp   = "sitofp"
  show LM_Ptrtoint = "ptrtoint"
  show LM_Inttoptr = "inttoptr"
  show LM_Bitcast  = "bitcast"


-- -----------------------------------------------------------------------------
-- * Floating point conversion
--

-- | Convert a Haskell Double to an LLVM hex encoded floating point form. In
-- Llvm float literals can be printed in a big-endian hexadecimal format,
-- regardless of underlying architecture.
--
-- See Note [LLVM Float Types].
dToStr :: Double -> String
dToStr d
  = let bs     = doubleToBytes d
        hex d' = case showHex d' "" of
                     []    -> error "dToStr: too few hex digits for float"
                     [x]   -> ['0',x]
                     [x,y] -> [x,y]
                     _     -> error "dToStr: too many hex digits for float"

        str  = map toUpper $ concat . fixEndian . (map hex) $ bs
    in  "0x" ++ str

-- Note [LLVM Float Types]
-- ~~~~~~~~~~~~~~~~~~~~~~~
-- We use 'dToStr' for both printing Float and Double floating point types. This is
-- as LLVM expects all floating point constants (single & double) to be in IEEE
-- 754 Double precision format. However, for single precision numbers (Float)
-- they should be *representable* in IEEE 754 Single precision format. So the
-- easiest way to do this is to narrow and widen again.
-- (i.e., Double -> Float -> Double). We must be careful doing this that GHC
-- doesn't optimize that away.

-- Note [narrowFp & widenFp]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- NOTE: we use float2Double & co directly as GHC likes to optimize away
-- successive calls of 'realToFrac', defeating the narrowing. (Bug #7600).
-- 'realToFrac' has inconsistent behaviour with optimisation as well that can
-- also cause issues, these methods don't.

narrowFp :: Double -> Float
{-# NOINLINE narrowFp #-}
narrowFp = double2Float

widenFp :: Float -> Double
{-# NOINLINE widenFp #-}
widenFp = float2Double

-- | Reverse or leave byte data alone to fix endianness on this target.
fixEndian :: [a] -> [a]
#ifdef WORDS_BIGENDIAN
fixEndian = id
#else
fixEndian = reverse
#endif

