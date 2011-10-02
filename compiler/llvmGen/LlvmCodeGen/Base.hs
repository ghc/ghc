-- ----------------------------------------------------------------------------
-- | Base LLVM Code Generation module
--
-- Contains functions useful through out the code generator.
--

module LlvmCodeGen.Base (

        LlvmCmmDecl, LlvmBasicBlock,
        LlvmUnresData, LlvmData, UnresLabel, UnresStatic,

        LlvmVersion, defaultLlvmVersion,

        LlvmEnv, initLlvmEnv, clearVars, varLookup, varInsert,
        funLookup, funInsert, getLlvmVer, setLlvmVer, getLlvmPlatform,

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
import Config
import Constants
import FastString
import OldCmm
import qualified Outputable as Outp
import Platform
import UniqFM
import Unique

-- ----------------------------------------------------------------------------
-- * Some Data Types
--

type LlvmCmmDecl = GenCmmDecl [LlvmData] (Maybe CmmStatics) (ListGraph LlvmStatement)
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
llvmGhcCC | cGhcUnregisterised == "NO" = CC_Ncc 10
          | otherwise                  = CC_Ccc

-- | Llvm Function type for Cmm function
llvmFunTy :: LlvmType
llvmFunTy = LMFunction $ llvmFunSig' (fsLit "a") ExternallyVisible

-- | Llvm Function signature
llvmFunSig :: LlvmEnv -> CLabel -> LlvmLinkageType -> LlvmFunctionDecl
llvmFunSig env lbl link = llvmFunSig' (strCLabel_llvm env lbl) link

llvmFunSig' :: LMString -> LlvmLinkageType -> LlvmFunctionDecl
llvmFunSig' lbl link
  = let toParams x | isPointer x = (x, [NoAlias, NoCapture])
                   | otherwise   = (x, [])
    in LlvmFunctionDecl lbl link llvmGhcCC LMVoid FixedArgs
                        (map (toParams . getVarType) llvmFunArgs) llvmFunAlign

-- | Create a Haskell function in LLVM.
mkLlvmFunc :: LlvmEnv -> CLabel -> LlvmLinkageType -> LMSection -> LlvmBlocks
           -> LlvmFunction
mkLlvmFunc env lbl link sec blks
  = let funDec = llvmFunSig env lbl link
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
-- * Llvm Version
--

-- | LLVM Version Number
type LlvmVersion = Int

-- | The LLVM Version we assume if we don't know
defaultLlvmVersion :: LlvmVersion
defaultLlvmVersion = 28

-- ----------------------------------------------------------------------------
-- * Environment Handling
--

-- two maps, one for functions and one for local vars.
newtype LlvmEnv = LlvmEnv (LlvmEnvMap, LlvmEnvMap, LlvmVersion, Platform)
type LlvmEnvMap = UniqFM LlvmType

-- | Get initial Llvm environment.
initLlvmEnv :: Platform -> LlvmEnv
initLlvmEnv platform = LlvmEnv (emptyUFM, emptyUFM, defaultLlvmVersion, platform)

-- | Clear variables from the environment.
clearVars :: LlvmEnv -> LlvmEnv
clearVars (LlvmEnv (e1, _, n, p)) = LlvmEnv (e1, emptyUFM, n, p)

-- | Insert functions into the environment.
varInsert, funInsert :: Uniquable key => key -> LlvmType -> LlvmEnv -> LlvmEnv
varInsert s t (LlvmEnv (e1, e2, n, p)) = LlvmEnv (e1, addToUFM e2 s t, n, p)
funInsert s t (LlvmEnv (e1, e2, n, p)) = LlvmEnv (addToUFM e1 s t, e2, n, p)

-- | Lookup functions in the environment.
varLookup, funLookup :: Uniquable key => key -> LlvmEnv -> Maybe LlvmType
varLookup s (LlvmEnv (_, e2, _, _)) = lookupUFM e2 s
funLookup s (LlvmEnv (e1, _, _, _)) = lookupUFM e1 s

-- | Get the LLVM version we are generating code for
getLlvmVer :: LlvmEnv -> LlvmVersion
getLlvmVer (LlvmEnv (_, _, n, _)) = n

-- | Set the LLVM version we are generating code for
setLlvmVer :: LlvmVersion -> LlvmEnv -> LlvmEnv
setLlvmVer n (LlvmEnv (e1, e2, _, p)) = LlvmEnv (e1, e2, n, p)

-- | Get the platform we are generating code for
getLlvmPlatform :: LlvmEnv -> Platform
getLlvmPlatform (LlvmEnv (_, _, _, p)) = p

-- ----------------------------------------------------------------------------
-- * Label handling
--

-- | Pretty print a 'CLabel'.
strCLabel_llvm :: LlvmEnv -> CLabel -> LMString
strCLabel_llvm env l
    = (fsLit . show . llvmSDoc . pprCLabel (getLlvmPlatform env)) l

-- | Create an external definition for a 'CLabel' defined in another module.
genCmmLabelRef :: LlvmEnv -> CLabel -> LMGlobal
genCmmLabelRef env = genStringLabelRef . strCLabel_llvm env

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

