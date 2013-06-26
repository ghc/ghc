-- ----------------------------------------------------------------------------
-- | Handle conversion of CmmData to LLVM code.
--

module LlvmCodeGen.Data (
        genLlvmData, resolveLlvmDatas, resolveLlvmData
    ) where

#include "HsVersions.h"

import Llvm
import LlvmCodeGen.Base

import BlockId
import CLabel
import Cmm

import FastString
import qualified Outputable

import Data.List (foldl')

-- ----------------------------------------------------------------------------
-- * Constants
--

-- | The string appended to a variable name to create its structure type alias
structStr :: LMString
structStr = fsLit "_struct"

-- ----------------------------------------------------------------------------
-- * Top level
--

-- | Pass a CmmStatic section to an equivalent Llvm code. Can't
-- complete this completely though as we need to pass all CmmStatic
-- sections before all references can be resolved. This last step is
-- done by 'resolveLlvmData'.
genLlvmData :: LlvmEnv -> (Section, CmmStatics) -> LlvmUnresData
genLlvmData env (sec, Statics lbl xs) =
    let dflags  = getDflags env
        static  = map genData xs
        label   = strCLabel_llvm env lbl

        types   = map getStatTypes static
        getStatTypes (Left  x) = cmmToLlvmType $ cmmLitType dflags x
        getStatTypes (Right x) = getStatType x

        strucTy = LMStruct types
        alias   = LMAlias ((label `appendFS` structStr), strucTy)
    in (lbl, sec, alias, static)


resolveLlvmDatas ::  LlvmEnv -> [LlvmUnresData] -> (LlvmEnv, [LlvmData])
resolveLlvmDatas env ldata
  = foldl' res (env, []) ldata
  where res (e, xs) ll =
            let (e', nd) = resolveLlvmData e ll
            in (e', nd:xs)

-- | Fix up CLabel references now that we should have passed all CmmData.
resolveLlvmData :: LlvmEnv -> LlvmUnresData -> (LlvmEnv, LlvmData)
resolveLlvmData env (lbl, sec, alias, unres) =
    let (env', static, refs) = resDatas env unres ([], [])
        struct         = Just $ LMStaticStruc static alias
        label          = strCLabel_llvm env lbl
        link           = if (externallyVisibleCLabel lbl)
                            then ExternallyVisible else Internal
        const          = if isSecConstant sec then Constant else Global
        glob           = LMGlobalVar label alias link Nothing Nothing const
    in (env', ((LMGlobal glob struct):refs, [alias]))

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
-- ** Resolve Data/CLabel references
--

-- | Resolve data list
resDatas :: LlvmEnv -> [UnresStatic] -> ([LlvmStatic], [LMGlobal])
         -> (LlvmEnv, [LlvmStatic], [LMGlobal])

resDatas env [] (stats, glob)
  = (env, stats, glob)

resDatas env (cmm:rest) (stats, globs)
  = let (env', nstat, nglob) = resData env cmm
    in resDatas env' rest (stats ++ [nstat], globs ++ nglob)

-- | Resolve an individual static label if it needs to be.
--
-- We check the 'LlvmEnv' to see if the reference has been defined in this
-- module. If it has we can retrieve its type and make a pointer, otherwise
-- we introduce a generic external definition for the referenced label and
-- then make a pointer.
resData :: LlvmEnv -> UnresStatic -> (LlvmEnv, LlvmStatic, [LMGlobal])

resData env (Right stat) = (env, stat, [])

resData env (Left cmm@(CmmLabel l)) =
    let dflags = getDflags env
        label = strCLabel_llvm env l
        ty = funLookup label env
        lmty = cmmToLlvmType $ cmmLitType dflags cmm
    in case ty of
            -- Make generic external label defenition and then pointer to it
            Nothing ->
                let glob@(LMGlobal var _) = genStringLabelRef dflags label
                    env' =  funInsert label (pLower $ getVarType var) env
                    ptr  = LMStaticPointer var
                in  (env', LMPtoI ptr lmty, [glob])
            -- Referenced data exists in this module, retrieve type and make
            -- pointer to it.
            Just ty' ->
                let var = LMGlobalVar label (LMPointer ty')
                            ExternallyVisible Nothing Nothing Global
                    ptr  = LMStaticPointer var
                in (env, LMPtoI ptr lmty, [])

resData env (Left (CmmLabelOff label off)) =
    let dflags = getDflags env
        (env', var, glob) = resData env (Left (CmmLabel label))
        offset = LMStaticLit $ LMIntLit (toInteger off) (llvmWord dflags)
    in (env', LMAdd var offset, glob)

resData env (Left (CmmLabelDiffOff l1 l2 off)) =
    let dflags = getDflags env
        (env1, var1, glob1) = resData env (Left (CmmLabel l1))
        (env2, var2, glob2) = resData env1 (Left (CmmLabel l2))
        var = LMSub var1 var2
        offset = LMStaticLit $ LMIntLit (toInteger off) (llvmWord dflags)
    in (env2, LMAdd var offset, glob1 ++ glob2)

resData _ _ = panic "resData: Non CLabel expr as left type!"

-- ----------------------------------------------------------------------------
-- * Generate static data
--

-- | Handle static data
genData :: CmmStatic -> UnresStatic

genData (CmmString str) =
    let v  = map (\x -> LMStaticLit $ LMIntLit (fromIntegral x) i8) str
        ve = v ++ [LMStaticLit $ LMIntLit 0 i8]
    in Right $ LMStaticArray ve (LMArray (length ve) i8)

genData (CmmUninitialised bytes)
    = Right $ LMUninitType (LMArray bytes i8)

genData (CmmStaticLit lit)
    = genStaticLit lit

-- | Generate Llvm code for a static literal.
--
-- Will either generate the code or leave it unresolved if it is a 'CLabel'
-- which isn't yet known.
genStaticLit :: CmmLit -> UnresStatic
genStaticLit (CmmInt i w)
    = Right $ LMStaticLit (LMIntLit i (LMInt $ widthInBits w))

genStaticLit (CmmFloat r w)
    = Right $ LMStaticLit (LMFloatLit (fromRational r) (widthToLlvmFloat w))

genStaticLit (CmmVec ls)
    = Right $ LMStaticLit (LMVectorLit (map toLlvmLit ls))
  where
    toLlvmLit :: CmmLit -> LlvmLit
    toLlvmLit lit = case genStaticLit lit of
                   Right (LMStaticLit llvmLit) -> llvmLit
                   _ -> panic "genStaticLit"

-- Leave unresolved, will fix later
genStaticLit c@(CmmLabel        _    ) = Left $ c
genStaticLit c@(CmmLabelOff     _   _) = Left $ c
genStaticLit c@(CmmLabelDiffOff _ _ _) = Left $ c

genStaticLit (CmmBlock b) = Left $ CmmLabel $ infoTblLbl b

genStaticLit (CmmHighStackMark)
    = panic "genStaticLit: CmmHighStackMark unsupported!"

-- -----------------------------------------------------------------------------
-- * Misc
--

-- | Error Function
panic :: String -> a
panic s = Outputable.panic $ "LlvmCodeGen.Data." ++ s

