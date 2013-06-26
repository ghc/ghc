-- ----------------------------------------------------------------------------
-- | Handle conversion of CmmData to LLVM code.
--

module LlvmCodeGen.Data (
        genLlvmData
    ) where

#include "HsVersions.h"

import Llvm
import LlvmCodeGen.Base

import BlockId
import CLabel
import Cmm

import FastString
import qualified Outputable

-- ----------------------------------------------------------------------------
-- * Constants
--

-- | The string appended to a variable name to create its structure type alias
structStr :: LMString
structStr = fsLit "_struct"

-- ----------------------------------------------------------------------------
-- * Top level
--

-- | Pass a CmmStatic section to an equivalent Llvm code.
genLlvmData :: (Section, CmmStatics) -> LlvmM LlvmData
genLlvmData (sec, Statics lbl xs) = do
    label <- strCLabel_llvm lbl
    static <- mapM genData xs
    let types   = map getStatType static

        strucTy = LMStruct types
        alias   = LMAlias ((label `appendFS` structStr), strucTy)

        struct         = Just $ LMStaticStruc static alias
        link           = if (externallyVisibleCLabel lbl)
                            then ExternallyVisible else Internal
        const          = if isSecConstant sec then Constant else Global
        glob           = LMGlobalVar label alias link Nothing Nothing const

    return ([LMGlobal glob struct], [alias])

-- | Should a data in this section be considered constant
isSecConstant :: Section -> Bool
isSecConstant Text                    = True
isSecConstant ReadOnlyData            = True
isSecConstant RelocatableReadOnlyData = True
isSecConstant ReadOnlyData16          = True
isSecConstant Data                    = False
isSecConstant UninitialisedData       = False
isSecConstant (OtherSection _)        = False


-- ----------------------------------------------------------------------------
-- * Generate static data
--

-- | Handle static data
genData :: CmmStatic -> LlvmM LlvmStatic

genData (CmmString str) = do
    let v  = map (\x -> LMStaticLit $ LMIntLit (fromIntegral x) i8) str
        ve = v ++ [LMStaticLit $ LMIntLit 0 i8]
    return $ LMStaticArray ve (LMArray (length ve) i8)

genData (CmmUninitialised bytes)
    = return $ LMUninitType (LMArray bytes i8)

genData (CmmStaticLit lit)
    = genStaticLit lit

-- | Generate Llvm code for a static literal.
--
-- Will either generate the code or leave it unresolved if it is a 'CLabel'
-- which isn't yet known.
genStaticLit :: CmmLit -> LlvmM LlvmStatic
genStaticLit (CmmInt i w)
    = return $ LMStaticLit (LMIntLit i (LMInt $ widthInBits w))

genStaticLit (CmmFloat r w)
    = return $ LMStaticLit (LMFloatLit (fromRational r) (widthToLlvmFloat w))

genStaticLit (CmmVec ls)
    = do sls <- mapM toLlvmLit ls
         return $ LMStaticLit (LMVectorLit sls)
  where
    toLlvmLit :: CmmLit -> LlvmM LlvmLit
    toLlvmLit lit = do
      slit <- genStaticLit lit
      case slit of
        LMStaticLit llvmLit -> return llvmLit
        _ -> panic "genStaticLit"

-- Leave unresolved, will fix later
genStaticLit cmm@(CmmLabel l) = do
    var <- getGlobalPtr =<< strCLabel_llvm l
    dflags <- getDynFlags
    let ptr = LMStaticPointer var
        lmty = cmmToLlvmType $ cmmLitType dflags cmm
    return $ LMPtoI ptr lmty

genStaticLit (CmmLabelOff label off) = do
    dflags <- getDynFlags
    var <- genStaticLit (CmmLabel label)
    let offset = LMStaticLit $ LMIntLit (toInteger off) (llvmWord dflags)
    return $ LMAdd var offset

genStaticLit (CmmLabelDiffOff l1 l2 off) = do
    dflags <- getDynFlags
    var1 <- genStaticLit (CmmLabel l1)
    var2 <- genStaticLit (CmmLabel l2)
    let var = LMSub var1 var2
        offset = LMStaticLit $ LMIntLit (toInteger off) (llvmWord dflags)
    return $ LMAdd var offset

genStaticLit (CmmBlock b) = genStaticLit $ CmmLabel $ infoTblLbl b

genStaticLit (CmmHighStackMark)
    = panic "genStaticLit: CmmHighStackMark unsupported!"

-- -----------------------------------------------------------------------------
-- * Misc
--

-- | Error Function
panic :: String -> a
panic s = Outputable.panic $ "LlvmCodeGen.Data." ++ s

