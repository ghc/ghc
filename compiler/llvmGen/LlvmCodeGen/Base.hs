-- ----------------------------------------------------------------------------
-- | Base LLVM Code Generation module
--
-- Contains functions useful through out the code generator.
--

module LlvmCodeGen.Base (

        LlvmCmmTop, LlvmBasicBlock,
        LlvmUnresData, LlvmData, UnresLabel, UnresStatic,

        LlvmEnv, initLlvmEnv, clearVars, varLookup, varInsert,
        funLookup, funInsert,

        cmmToLlvmType, widthToLlvmFloat, widthToLlvmInt, llvmFunTy,
        llvmFunSig, llvmStdFunAttrs, llvmFunAlign, llvmInfAlign,
        llvmPtrBits, mkLlvmFunc, tysToParams,

        strCLabel_llvm, genCmmLabelRef, genStringLabelRef

    ) where

#include "HsVersions.h"

import Llvm
import LlvmCodeGen.Regs

import CLabel
import CgUtils ( activeStgRegs )
import Cmm
import Constants
import FastString
import qualified Outputable as Outp
import UniqFM
import Unique

-- ----------------------------------------------------------------------------
-- * Some Data Types
--

type LlvmCmmTop = GenCmmTop LlvmData [CmmStatic] (ListGraph LlvmStatement)
type LlvmBasicBlock = GenBasicBlock LlvmStatement

-- | Unresolved code.
-- Of the form: (data label, data type, unresolved data)
type LlvmUnresData = (CLabel, Section, LlvmType, [UnresStatic])

-- | Top level LLVM Data (globals and type aliases)
type LlvmData = ([LMGlobal], [LlvmType])

-- | An unresolved Label.
--
-- Labels are unresolved when we haven't yet determined if they are defined in
-- the module we are currently compiling, or an external one.
type UnresLabel  = CmmLit
type UnresStatic = Either UnresLabel LlvmStatic

-- ----------------------------------------------------------------------------
-- * Type translations
--

-- | Translate a basic CmmType to an LlvmType.
cmmToLlvmType :: CmmType -> LlvmType
cmmToLlvmType ty | isFloatType ty = widthToLlvmFloat $ typeWidth ty
                 | otherwise      = widthToLlvmInt   $ typeWidth ty

-- | Translate a Cmm Float Width to a LlvmType.
widthToLlvmFloat :: Width -> LlvmType
widthToLlvmFloat W32  = LMFloat
widthToLlvmFloat W64  = LMDouble
widthToLlvmFloat W80  = LMFloat80
widthToLlvmFloat W128 = LMFloat128
widthToLlvmFloat w    = panic $ "widthToLlvmFloat: Bad float size: " ++ show w

-- | Translate a Cmm Bit Width to a LlvmType.
widthToLlvmInt :: Width -> LlvmType
widthToLlvmInt w = LMInt $ widthInBits w

-- | GHC Call Convention for LLVM
llvmGhcCC :: LlvmCallConvention
llvmGhcCC = CC_Ncc 10

-- | Llvm Function type for Cmm function
llvmFunTy :: LlvmType
llvmFunTy = LMFunction $ llvmFunSig' (fsLit "a") ExternallyVisible

-- | Llvm Function signature
llvmFunSig :: CLabel -> LlvmLinkageType -> LlvmFunctionDecl
llvmFunSig lbl link = llvmFunSig' (strCLabel_llvm lbl) link

llvmFunSig' :: LMString -> LlvmLinkageType -> LlvmFunctionDecl
llvmFunSig' lbl link
  = let toParams x | isPointer x = (x, [NoAlias, NoCapture])
                   | otherwise   = (x, [])
    in LlvmFunctionDecl lbl link llvmGhcCC LMVoid FixedArgs
                        (map (toParams . getVarType) llvmFunArgs) llvmFunAlign

-- | Create a Haskell function in LLVM.
mkLlvmFunc :: CLabel -> LlvmLinkageType -> LMSection -> LlvmBlocks
           -> LlvmFunction
mkLlvmFunc lbl link sec blks
  = let funDec = llvmFunSig lbl link
        funArgs = map (fsLit . getPlainName) llvmFunArgs
    in LlvmFunction funDec funArgs llvmStdFunAttrs sec blks

-- | Alignment to use for functions
llvmFunAlign :: LMAlign
llvmFunAlign = Just wORD_SIZE

-- | Alignment to use for into tables
llvmInfAlign :: LMAlign
llvmInfAlign = Just wORD_SIZE

-- | A Function's arguments
llvmFunArgs :: [LlvmVar]
llvmFunArgs = map lmGlobalRegArg activeStgRegs

-- | Llvm standard fun attributes
llvmStdFunAttrs :: [LlvmFuncAttr]
llvmStdFunAttrs = [NoUnwind]

-- | Convert a list of types to a list of function parameters
-- (each with no parameter attributes)
tysToParams :: [LlvmType] -> [LlvmParameter]
tysToParams = map (\ty -> (ty, []))

-- | Pointer width
llvmPtrBits :: Int
llvmPtrBits = widthInBits $ typeWidth gcWord


-- ----------------------------------------------------------------------------
-- * Environment Handling
--

type LlvmEnvMap = UniqFM LlvmType
-- two maps, one for functions and one for local vars.
type LlvmEnv = (LlvmEnvMap, LlvmEnvMap)

-- | Get initial Llvm environment.
initLlvmEnv :: LlvmEnv
initLlvmEnv = (emptyUFM, emptyUFM)

-- | Clear variables from the environment.
clearVars :: LlvmEnv -> LlvmEnv
clearVars (e1, _) = (e1, emptyUFM)

-- | Insert functions into the environment.
varInsert, funInsert :: Uniquable key => key -> LlvmType -> LlvmEnv -> LlvmEnv
varInsert s t (e1, e2) = (e1, addToUFM e2 s t)
funInsert s t (e1, e2) = (addToUFM e1 s t, e2)

-- | Lookup functions in the environment.
varLookup, funLookup :: Uniquable key => key -> LlvmEnv -> Maybe LlvmType
varLookup s (_, e2) = lookupUFM e2 s
funLookup s (e1, _) = lookupUFM e1 s


-- ----------------------------------------------------------------------------
-- * Label handling
--

-- | Pretty print a 'CLabel'.
strCLabel_llvm :: CLabel -> LMString
strCLabel_llvm l = (fsLit . show . llvmSDoc . pprCLabel) l

-- | Create an external definition for a 'CLabel' defined in another module.
genCmmLabelRef :: CLabel -> LMGlobal
genCmmLabelRef = genStringLabelRef . strCLabel_llvm

-- | As above ('genCmmLabelRef') but taking a 'LMString', not 'CLabel'.
genStringLabelRef :: LMString -> LMGlobal
genStringLabelRef cl
  = let ty = LMPointer $ LMArray 0 llvmWord
    in (LMGlobalVar cl ty External Nothing Nothing False, Nothing)


-- ----------------------------------------------------------------------------
-- * Misc
--

-- | Error function
panic :: String -> a
panic s = Outp.panic $ "LlvmCodeGen.Base." ++ s

