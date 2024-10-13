
-- ----------------------------------------------------------------------------
-- | Handle conversion of CmmData to LLVM code.
--

module GHC.CmmToLlvm.Data (
        genLlvmData, genData
    ) where

import GHC.Prelude

import GHC.Llvm
import GHC.Llvm.Types (widenFp)
import GHC.CmmToLlvm.Base
import GHC.CmmToLlvm.Config

import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.Cmm.InitFini
import GHC.Cmm
import GHC.Platform

import GHC.Data.FastString
import GHC.Utils.Panic
import qualified Data.ByteString as BS

-- ----------------------------------------------------------------------------
-- * Constants
--

-- | The string appended to a variable name to create its structure type alias
structStr :: LMString
structStr = fsLit "_struct"

-- | The LLVM visibility of the label
linkage :: CLabel -> LlvmLinkageType
linkage lbl = if externallyVisibleCLabel lbl
              then ExternallyVisible else Internal

-- ----------------------------------------------------------------------------
-- * Top level
--

-- | Pass a CmmStatic section to an equivalent Llvm code.
genLlvmData :: (Section, RawCmmStatics) -> LlvmM LlvmData
-- See Note [emit-time elimination of static indirections] in "GHC.Cmm.CLabel".
genLlvmData (_, CmmStaticsRaw alias [CmmStaticLit (CmmLabel lbl), CmmStaticLit ind, _, _])
  | lbl == mkIndStaticInfoLabel
  , let labelInd (CmmLabelOff l _) = Just l
        labelInd (CmmLabel l) = Just l
        labelInd _ = Nothing
  , Just ind' <- labelInd ind
  , alias `mayRedirectTo` ind' = do
    label <- strCLabel_llvm alias
    label' <- strCLabel_llvm ind'
    let link     = linkage alias
        link'    = linkage ind'
        -- the LLVM type we give the alias is an empty struct type
        -- but it doesn't really matter, as the pointer is only
        -- used for (bit/int)casting.
        tyAlias  = LMAlias (label `appendFS` structStr, LMStructU [])

        aliasDef = LMGlobalVar label tyAlias link Nothing Nothing Alias
        -- we don't know the type of the indirectee here
        indType  = panic "will be filled by 'aliasify', later"
        orig     = LMStaticPointer $ LMGlobalVar label' indType link' Nothing Nothing Alias

    pure ([LMGlobal aliasDef $ Just orig], [tyAlias])

-- See Note [Initializers and finalizers in Cmm] in GHC.Cmm.InitFini.
genLlvmData (sect, statics)
  | Just (initOrFini, clbls) <- isInitOrFiniArray (CmmData sect statics)
  = let var = case initOrFini of
                IsInitArray -> fsLit "llvm.global_ctors"
                IsFiniArray -> fsLit "llvm.global_dtors"
    in genGlobalLabelArray var clbls

genLlvmData (sec, CmmStaticsRaw lbl xs) = do
    label <- strCLabel_llvm lbl
    static <- mapM genData xs
    lmsec <- llvmSection sec
    platform <- getPlatform
    let types   = map getStatType static

        strucTy = LMStruct types
        tyAlias = LMAlias (label `appendFS` structStr, strucTy)

        struct         = Just $ LMStaticStruc static tyAlias
        link           = linkage lbl
        align          = case sec of
                            Section CString _ -> if (platformArch platform == ArchS390X)
                                                    then Just 2 else Just 1
                            Section Data _    -> Just $ platformWordSizeInBytes platform
                            _                 -> Nothing
        const          = if sectionProtection sec == ReadOnlySection
                            then Constant else Global
        varDef         = LMGlobalVar label tyAlias link lmsec align const
        globDef        = LMGlobal varDef struct

    return ([globDef], [tyAlias])

-- | Produce an initializer or finalizer array declaration.
-- See Note [Initializers and finalizers in Cmm] in GHC.Cmm.InitFini for
-- details.
genGlobalLabelArray :: FastString -> [CLabel] -> LlvmM LlvmData
genGlobalLabelArray var_nm clbls = do
    lbls <- mapM strCLabel_llvm clbls
    decls <- mapM mkFunDecl lbls
    let entries = map toArrayEntry lbls
        static = LMStaticArray entries arr_ty
        arr = LMGlobal arr_var (Just static)
    return ([arr], decls)
  where
    mkFunDecl :: LMString -> LlvmM LlvmType
    mkFunDecl fn_lbl = do
        let fn_ty = mkFunTy fn_lbl
        funInsert fn_lbl fn_ty
        return (fn_ty)

    toArrayEntry :: LMString -> LlvmStatic
    toArrayEntry fn_lbl =
        let fn_var = LMGlobalVar fn_lbl (LMPointer $ mkFunTy fn_lbl) Internal Nothing Nothing Global
            fn = LMStaticPointer fn_var
            null = LMStaticLit (LMNullLit i8Ptr)
            prio = LMStaticLit $ LMIntLit 0xffff i32
        in LMStaticStrucU [prio, fn, null] entry_ty

    arr_var = LMGlobalVar var_nm arr_ty Internal Nothing Nothing Global
    mkFunTy lbl = LMFunction $ LlvmFunctionDecl lbl ExternallyVisible CC_Ccc LMVoid FixedArgs [] Nothing
    entry_ty = LMStructU [i32, LMPointer $ mkFunTy $ fsLit "placeholder", LMPointer i8]
    arr_ty = LMArray (length clbls) entry_ty

-- | Format the section type part of a Cmm Section
llvmSectionType :: Platform -> SectionType -> FastString
llvmSectionType p t = case t of
    Text                    -> fsLit ".text"
    ReadOnlyData            -> case platformOS p of
                                 OSMinGW32 -> fsLit ".rdata"
                                 _         -> fsLit ".rodata"
    RelocatableReadOnlyData -> case platformOS p of
                                 OSMinGW32 -> fsLit ".rdata$rel.ro"
                                 _         -> fsLit ".data.rel.ro"
    Data                    -> fsLit ".data"
    UninitialisedData       -> fsLit ".bss"
    CString                 -> case platformOS p of
                                 OSMinGW32 -> fsLit ".rdata$str"
                                 _         -> fsLit ".rodata.str"

    InitArray               -> panic "llvmSectionType: InitArray"
    FiniArray               -> panic "llvmSectionType: FiniArray"
    OtherSection _          -> panic "llvmSectionType: unknown section type"

-- | Format a Cmm Section into a LLVM section name
llvmSection :: Section -> LlvmM LMSection
llvmSection (Section t suffix) = do
  opts <- getConfig
  let splitSect = llvmCgSplitSection opts
      platform  = llvmCgPlatform     opts
  if not splitSect
  then return Nothing
  else do
    lmsuffix <- strCLabel_llvm suffix
    let result sep = Just (concatFS [llvmSectionType platform t
                                    , fsLit sep, lmsuffix])
    case platformOS platform of
      OSMinGW32 -> return (result "$")
      _         -> return (result ".")

-- ----------------------------------------------------------------------------
-- * Generate static data
--

-- | Handle static data
genData :: CmmStatic -> LlvmM LlvmStatic

genData (CmmFileEmbed {}) = panic "Unexpected CmmFileEmbed literal"
genData (CmmString str) = do
    let v  = map (\x -> LMStaticLit $ LMIntLit (fromIntegral x) i8)
                 (BS.unpack str)
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

genStaticLit (CmmFloat r W32)
    = return $ LMStaticLit (LMFloatLit (widenFp (fromRational r :: Float)) (widthToLlvmFloat W32))

genStaticLit (CmmFloat r W64)
    = return $ LMStaticLit (LMFloatLit (fromRational r :: Double) (widthToLlvmFloat W64))

genStaticLit (CmmFloat _r _w)
    = panic "genStaticLit (CmmLit:CmmFloat), unsupported float lit"

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
    platform <- getPlatform
    let ptr = LMStaticPointer var
        lmty = cmmToLlvmType $ cmmLitType platform cmm
    return $ LMPtoI ptr lmty

genStaticLit (CmmLabelOff label off) = do
    platform <- getPlatform
    var <- genStaticLit (CmmLabel label)
    let offset = LMStaticLit $ LMIntLit (toInteger off) (llvmWord platform)
    return $ LMAdd var offset

genStaticLit (CmmLabelDiffOff l1 l2 off w) = do
    platform <- getPlatform
    var1 <- genStaticLit (CmmLabel l1)
    var2 <- genStaticLit (CmmLabel l2)
    let var
          | w == wordWidth platform = LMSub var1 var2
          | otherwise = LMTrunc (LMSub var1 var2) (widthToLlvmInt w)
        offset = LMStaticLit $ LMIntLit (toInteger off) (LMInt $ widthInBits w)
    return $ LMAdd var offset

genStaticLit (CmmBlock b) = genStaticLit $ CmmLabel $ infoTblLbl b

genStaticLit (CmmHighStackMark)
    = panic "genStaticLit: CmmHighStackMark unsupported!"
