{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------------------------------------
-- | The LLVM Type System.
--

module Llvm.Types where

#include "HsVersions.h"

import Data.Char
import Data.Int
import Numeric

import DynFlags
import FastString
import Outputable
import Unique

-- from NCG
import PprBase

import GHC.Float

-- -----------------------------------------------------------------------------
-- * LLVM Basic Types and Variables
--

-- | A global mutable variable. Maybe defined or external
data LMGlobal = LMGlobal {
  getGlobalVar :: LlvmVar,          -- ^ Returns the variable of the 'LMGlobal'
  getGlobalValue :: Maybe LlvmStatic -- ^ Return the value of the 'LMGlobal'
  }

-- | A String in LLVM
type LMString = FastString

-- | A type alias
type LlvmAlias = (LMString, LlvmType)

-- | Llvm Types
data LlvmType
  = LMInt Int             -- ^ An integer with a given width in bits.
  | LMFloat               -- ^ 32 bit floating point
  | LMDouble              -- ^ 64 bit floating point
  | LMFloat80             -- ^ 80 bit (x86 only) floating point
  | LMFloat128            -- ^ 128 bit floating point
  | LMPointer LlvmType    -- ^ A pointer to a 'LlvmType'
  | LMArray Int LlvmType  -- ^ An array of 'LlvmType'
  | LMVector Int LlvmType -- ^ A vector of 'LlvmType'
  | LMLabel               -- ^ A 'LlvmVar' can represent a label (address)
  | LMVoid                -- ^ Void type
  | LMStruct [LlvmType]   -- ^ Structure type
  | LMAlias LlvmAlias     -- ^ A type alias
  | LMMetadata            -- ^ LLVM Metadata

  -- | Function type, used to create pointers to functions
  | LMFunction LlvmFunctionDecl
  deriving (Eq)

instance Outputable LlvmType where
  ppr (LMInt size     ) = char 'i' <> ppr size
  ppr (LMFloat        ) = text "float"
  ppr (LMDouble       ) = text "double"
  ppr (LMFloat80      ) = text "x86_fp80"
  ppr (LMFloat128     ) = text "fp128"
  ppr (LMPointer x    ) = ppr x <> char '*'
  ppr (LMArray nr tp  ) = char '[' <> ppr nr <> text " x " <> ppr tp <> char ']'
  ppr (LMVector nr tp ) = char '<' <> ppr nr <> text " x " <> ppr tp <> char '>'
  ppr (LMLabel        ) = text "label"
  ppr (LMVoid         ) = text "void"
  ppr (LMStruct tys   ) = text "<{" <> ppCommaJoin tys <> text "}>"
  ppr (LMMetadata     ) = text "metadata"

  ppr (LMFunction (LlvmFunctionDecl _ _ _ r varg p _))
    = ppr r <+> lparen <> ppParams varg p <> rparen

  ppr (LMAlias (s,_)) = char '%' <> ftext s

ppParams :: LlvmParameterListType -> [LlvmParameter] -> SDoc
ppParams varg p
  = let varg' = case varg of
          VarArgs | null args -> sLit "..."
                  | otherwise -> sLit ", ..."
          _otherwise          -> sLit ""
        -- by default we don't print param attributes
        args = map fst p
    in ppCommaJoin args <> ptext varg'

-- | An LLVM section definition. If Nothing then let LLVM decide the section
type LMSection = Maybe LMString
type LMAlign = Maybe Int

data LMConst = Global      -- ^ Mutable global variable
             | Constant    -- ^ Constant global variable
             | Alias       -- ^ Alias of another variable
             deriving (Eq)

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

instance Outputable LlvmVar where
  ppr (LMLitVar x)  = ppr x
  ppr (x         )  = ppr (getVarType x) <+> ppName x


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
  -- | Vector literal
  | LMVectorLit [LlvmLit]
  -- | Undefined value, random bit pattern. Useful for optimisations.
  | LMUndefLit LlvmType
  deriving (Eq)

instance Outputable LlvmLit where
  ppr l@(LMVectorLit {}) = ppLit l
  ppr l                  = ppr (getLitType l) <+> ppLit l


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

instance Outputable LlvmStatic where
  ppr (LMComment       s) = text "; " <> ftext s
  ppr (LMStaticLit   l  ) = ppr l
  ppr (LMUninitType    t) = ppr t <> text " undef"
  ppr (LMStaticStr   s t) = ppr t <> text " c\"" <> ftext s <> text "\\00\""
  ppr (LMStaticArray d t) = ppr t <> text " [" <> ppCommaJoin d <> char ']'
  ppr (LMStaticStruc d t) = ppr t <> text "<{" <> ppCommaJoin d <> text "}>"
  ppr (LMStaticPointer v) = ppr v
  ppr (LMBitc v t)
      = ppr t <> text " bitcast (" <> ppr v <> text " to " <> ppr t <> char ')'
  ppr (LMPtoI v t)
      = ppr t <> text " ptrtoint (" <> ppr v <> text " to " <> ppr t <> char ')'

  ppr (LMAdd s1 s2)
      = pprStaticArith s1 s2 (sLit "add") (sLit "fadd") "LMAdd"
  ppr (LMSub s1 s2)
      = pprStaticArith s1 s2 (sLit "sub") (sLit "fsub") "LMSub"

pprStaticArith :: LlvmStatic -> LlvmStatic -> LitString -> LitString -> String -> SDoc
pprStaticArith s1 s2 int_op float_op op_name =
  let ty1 = getStatType s1
      op  = if isFloat ty1 then float_op else int_op
  in if ty1 == getStatType s2
     then ppr ty1 <+> ptext op <+> lparen <> ppr s1 <> comma <> ppr s2 <> rparen
     else sdocWithDynFlags $ \dflags ->
            error $ op_name ++ " with different types! s1: "
                    ++ showSDoc dflags (ppr s1) ++ ", s2: " ++ showSDoc dflags (ppr s2)

-- -----------------------------------------------------------------------------
-- ** Operations on LLVM Basic Types and Variables
--

-- | Return the variable name or value of the 'LlvmVar'
-- in Llvm IR textual representation (e.g. @\@x@, @%y@ or @42@).
ppName :: LlvmVar -> SDoc
ppName v@(LMGlobalVar {}) = char '@' <> ppPlainName v
ppName v@(LMLocalVar  {}) = char '%' <> ppPlainName v
ppName v@(LMNLocalVar {}) = char '%' <> ppPlainName v
ppName v@(LMLitVar    {}) =             ppPlainName v

-- | Return the variable name or value of the 'LlvmVar'
-- in a plain textual representation (e.g. @x@, @y@ or @42@).
ppPlainName :: LlvmVar -> SDoc
ppPlainName (LMGlobalVar x _ _ _ _ _) = ftext x
ppPlainName (LMLocalVar  x LMLabel  ) = text (show x)
ppPlainName (LMLocalVar  x _        ) = text ('l' : show x)
ppPlainName (LMNLocalVar x _        ) = ftext x
ppPlainName (LMLitVar    x          ) = ppLit x

-- | Print a literal value. No type.
ppLit :: LlvmLit -> SDoc
ppLit (LMIntLit i (LMInt 32))  = ppr (fromInteger i :: Int32)
ppLit (LMIntLit i (LMInt 64))  = ppr (fromInteger i :: Int64)
ppLit (LMIntLit   i _       )  = ppr ((fromInteger i)::Int)
ppLit (LMFloatLit r LMFloat )  = ppFloat $ narrowFp r
ppLit (LMFloatLit r LMDouble)  = ppDouble r
ppLit f@(LMFloatLit _ _)       = sdocWithDynFlags (\dflags ->
                                   error $ "Can't print this float literal!" ++ showSDoc dflags (ppr f))
ppLit (LMVectorLit ls  )       = char '<' <+> ppCommaJoin ls <+> char '>'
ppLit (LMNullLit _     )       = text "null"
ppLit (LMUndefLit _    )       = text "undef"

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
getLitType (LMVectorLit [])  = panic "getLitType"
getLitType (LMVectorLit ls)  = LMVector (length ls) (getLitType (head ls))
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

-- | Return the 'LlvmLinkageType' for a 'LlvmVar'
getLink :: LlvmVar -> LlvmLinkageType
getLink (LMGlobalVar _ _ l _ _ _) = l
getLink _                         = Internal

-- | Add a pointer indirection to the supplied type. 'LMLabel' and 'LMVoid'
-- cannot be lifted.
pLift :: LlvmType -> LlvmType
pLift LMLabel    = error "Labels are unliftable"
pLift LMVoid     = error "Voids are unliftable"
pLift LMMetadata = error "Metadatas are unliftable"
pLift x          = LMPointer x

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
pLower x  = error $ showSDoc undefined (ppr x) ++ " is a unlowerable type, need a pointer"

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

-- | Test if the given 'LlvmType' is an 'LMVector' construct
isVector :: LlvmType -> Bool
isVector (LMVector {}) = True
isVector _             = False

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
-- PMW: At least judging by the way LLVM outputs constants, pointers
--      should use the former, but arrays the latter.
llvmWidthInBits dflags (LMPointer _)   = llvmWidthInBits dflags (llvmWord dflags)
llvmWidthInBits dflags (LMArray n t)   = n * llvmWidthInBits dflags t
llvmWidthInBits dflags (LMVector n ty) = n * llvmWidthInBits dflags ty
llvmWidthInBits _      LMLabel         = 0
llvmWidthInBits _      LMVoid          = 0
llvmWidthInBits dflags (LMStruct tys)  = sum $ map (llvmWidthInBits dflags) tys
llvmWidthInBits _      (LMFunction  _) = 0
llvmWidthInBits dflags (LMAlias (_,t)) = llvmWidthInBits dflags t
llvmWidthInBits _      LMMetadata      = panic "llvmWidthInBits: Meta-data has no runtime representation!"


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

instance Outputable LlvmFunctionDecl where
  ppr (LlvmFunctionDecl n l c r varg p a)
    = let align = case a of
                       Just a' -> text " align " <> ppr a'
                       Nothing -> empty
      in ppr l <+> ppr c <+> ppr r <+> char '@' <> ftext n <>
             lparen <> ppParams varg p <> rparen <> align

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

instance Outputable LlvmParamAttr where
  ppr ZeroExt   = text "zeroext"
  ppr SignExt   = text "signext"
  ppr InReg     = text "inreg"
  ppr ByVal     = text "byval"
  ppr SRet      = text "sret"
  ppr NoAlias   = text "noalias"
  ppr NoCapture = text "nocapture"
  ppr Nest      = text "nest"

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

instance Outputable LlvmFuncAttr where
  ppr AlwaysInline       = text "alwaysinline"
  ppr InlineHint         = text "inlinehint"
  ppr NoInline           = text "noinline"
  ppr OptSize            = text "optsize"
  ppr NoReturn           = text "noreturn"
  ppr NoUnwind           = text "nounwind"
  ppr ReadNone           = text "readnon"
  ppr ReadOnly           = text "readonly"
  ppr Ssp                = text "ssp"
  ppr SspReq             = text "ssqreq"
  ppr NoRedZone          = text "noredzone"
  ppr NoImplicitFloat    = text "noimplicitfloat"
  ppr Naked              = text "naked"


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

instance Outputable LlvmCallConvention where
  ppr CC_Ccc       = text "ccc"
  ppr CC_Fastcc    = text "fastcc"
  ppr CC_Coldcc    = text "coldcc"
  ppr (CC_Ncc i)   = text "cc " <> ppr i
  ppr CC_X86_Stdcc = text "x86_stdcallcc"


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
  -- | Symbol is private to the module and should not appear in the symbol table
  | Private
  deriving (Eq)

instance Outputable LlvmLinkageType where
  ppr Internal          = text "internal"
  ppr LinkOnce          = text "linkonce"
  ppr Weak              = text "weak"
  ppr Appending         = text "appending"
  ppr ExternWeak        = text "extern_weak"
  -- ExternallyVisible does not have a textual representation, it is
  -- the linkage type a function resolves to if no other is specified
  -- in Llvm.
  ppr ExternallyVisible = empty
  ppr External          = text "external"
  ppr Private           = text "private"

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

instance Outputable LlvmMachOp where
  ppr LM_MO_Add  = text "add"
  ppr LM_MO_Sub  = text "sub"
  ppr LM_MO_Mul  = text "mul"
  ppr LM_MO_UDiv = text "udiv"
  ppr LM_MO_SDiv = text "sdiv"
  ppr LM_MO_URem = text "urem"
  ppr LM_MO_SRem = text "srem"
  ppr LM_MO_FAdd = text "fadd"
  ppr LM_MO_FSub = text "fsub"
  ppr LM_MO_FMul = text "fmul"
  ppr LM_MO_FDiv = text "fdiv"
  ppr LM_MO_FRem = text "frem"
  ppr LM_MO_Shl  = text "shl"
  ppr LM_MO_LShr = text "lshr"
  ppr LM_MO_AShr = text "ashr"
  ppr LM_MO_And  = text "and"
  ppr LM_MO_Or   = text "or"
  ppr LM_MO_Xor  = text "xor"


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

instance Outputable LlvmCmpOp where
  ppr LM_CMP_Eq  = text "eq"
  ppr LM_CMP_Ne  = text "ne"
  ppr LM_CMP_Ugt = text "ugt"
  ppr LM_CMP_Uge = text "uge"
  ppr LM_CMP_Ult = text "ult"
  ppr LM_CMP_Ule = text "ule"
  ppr LM_CMP_Sgt = text "sgt"
  ppr LM_CMP_Sge = text "sge"
  ppr LM_CMP_Slt = text "slt"
  ppr LM_CMP_Sle = text "sle"
  ppr LM_CMP_Feq = text "oeq"
  ppr LM_CMP_Fne = text "une"
  ppr LM_CMP_Fgt = text "ogt"
  ppr LM_CMP_Fge = text "oge"
  ppr LM_CMP_Flt = text "olt"
  ppr LM_CMP_Fle = text "ole"


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

instance Outputable LlvmCastOp where
  ppr LM_Trunc    = text "trunc"
  ppr LM_Zext     = text "zext"
  ppr LM_Sext     = text "sext"
  ppr LM_Fptrunc  = text "fptrunc"
  ppr LM_Fpext    = text "fpext"
  ppr LM_Fptoui   = text "fptoui"
  ppr LM_Fptosi   = text "fptosi"
  ppr LM_Uitofp   = text "uitofp"
  ppr LM_Sitofp   = text "sitofp"
  ppr LM_Ptrtoint = text "ptrtoint"
  ppr LM_Inttoptr = text "inttoptr"
  ppr LM_Bitcast  = text "bitcast"


-- -----------------------------------------------------------------------------
-- * Floating point conversion
--

-- | Convert a Haskell Double to an LLVM hex encoded floating point form. In
-- Llvm float literals can be printed in a big-endian hexadecimal format,
-- regardless of underlying architecture.
--
-- See Note [LLVM Float Types].
ppDouble :: Double -> SDoc
ppDouble d
  = let bs     = doubleToBytes d
        hex d' = case showHex d' "" of
                     []    -> error "dToStr: too few hex digits for float"
                     [x]   -> ['0',x]
                     [x,y] -> [x,y]
                     _     -> error "dToStr: too many hex digits for float"

        str  = map toUpper $ concat $ fixEndian $ map hex bs
    in  text "0x" <> text str

-- Note [LLVM Float Types]
-- ~~~~~~~~~~~~~~~~~~~~~~~
-- We use 'ppDouble' for both printing Float and Double floating point types. This is
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

ppFloat :: Float -> SDoc
ppFloat = ppDouble . widenFp

-- | Reverse or leave byte data alone to fix endianness on this target.
fixEndian :: [a] -> [a]
#ifdef WORDS_BIGENDIAN
fixEndian = id
#else
fixEndian = reverse
#endif


--------------------------------------------------------------------------------
-- * Misc functions
--------------------------------------------------------------------------------

ppCommaJoin :: (Outputable a) => [a] -> SDoc
ppCommaJoin strs = hsep $ punctuate comma (map ppr strs)

ppSpaceJoin :: (Outputable a) => [a] -> SDoc
ppSpaceJoin strs = hsep (map ppr strs)
