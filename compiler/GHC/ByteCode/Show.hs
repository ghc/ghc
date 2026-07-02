{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | […]
module GHC.ByteCode.Show (showByteCode) where

import Prelude ((+), (-), Integral, div)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((>>>))
import Control.Exception (assert)
import Data.Eq ((==))
import Data.Bits (FiniteBits, finiteBitSize)
import Data.Function (($), id, (.))
import Data.Tuple (fst, snd, uncurry)
import Data.Bool (Bool, otherwise, not, (&&))
import Data.Int (Int)
import Data.Word (Word)
import Data.Maybe (Maybe, maybe)
import Data.Either (Either, either)
import Data.List (length, (++), map, zipWith, take, drop, replicate)
import Data.String (String)
import Data.ByteString (ByteString, unpack)
import Data.ByteString.Short (ShortByteString)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap (toList)
import Data.Array (bounds, indices, elems)
import Numeric (showHex)
import Text.Show (show)
import System.IO (IO, FilePath)
import GHC.Data.Strict qualified as Strict (Maybe, maybe)
import GHC.Data.FastString (unpackFS)
import GHC.Data.FlatBag (FlatBag, elemsFlatBag)
import GHC.Fingerprint (Fingerprint)
import GHC.Types.SrcLoc (noSrcSpan)
import GHC.Types.Name (Name)
import GHC.Types.Name.Occurrence (OccName)
import GHC.Types.Tickish (BreakTickIndex, BreakpointId (..))
import GHC.Types.SptEntry (SptEntry (..))
import GHC.Types.Error (MessageClass (MCDump))
import GHC.Utils.Logger (Logger, logMsg)
import GHC.Utils.Binary (BinSrcSpan (..))
import GHC.Utils.Encoding.UTF8 (utf8DecodeShortByteString, utf8DecodeByteString)
import GHC.Utils.Outputable
       (
           defaultDumpStyle,
           SDoc,
           text,
           (<>),
           (<+>),
           hsep,
           vcat,
           hang,
           withPprStyle,
           ppr
       )
import GHC.Unit.Types (Module)
import GHC.Iface.Type (IfaceType, IfaceTvBndr, IfaceIdBndr)
import GHC.HsToCore.Breakpoints (ModBreaks (..))
import GHC.ByteCode.Types
       (
           FFIInfo (..),
           BCONPtr (..),
           BCOPtr (..),
           UnlinkedBCO (..),
           ByteCodeHpcInfo (..),
           CompiledByteCode (..)
       )
import GHC.ByteCode.Breakpoints
       (
           InternalBreakpointId (..),
           InternalBreakLoc (..),
           CgBreakInfo (..),
           InternalModBreaks (..)
       )
import GHC.ByteCode.Binary (OnDiskModuleByteCode (..))
import GHC.ByteCode.Serialize (readOnDiskModuleByteCode)
import GHC.Driver.Env.Types (HscEnv)
import GHCi.FFI (FFIType)
import GHCi.Message (ConInfoTable (..))

-- | […]
showByteCode :: Logger -> HscEnv -> FilePath -> IO ()
showByteCode logger env path = do
    byteCode <- readOnDiskModuleByteCode env path
    logMsg logger
           MCDump
           noSrcSpan
           (withPprStyle defaultDumpStyle $ pprOnDiskModuleByteCode byteCode)

-- | […]
pprOnDiskModuleByteCode :: OnDiskModuleByteCode -> SDoc
pprOnDiskModuleByteCode OnDiskModuleByteCode {..}
    = vcat [
               pprModuleIdent                   $ odgbc_module,
               pprOnDiskModuleByteCodeHash      $ odgbc_hash,
               pprCompiledByteCode odgbc_module $ odgbc_compiled_byte_code,
               pprObjectFileContents            $ odgbc_foreign
           ]

-- | […]
pprModuleIdent :: Module -> SDoc
pprModuleIdent = entry (text "name:") . ppr

-- | […]
pprOnDiskModuleByteCodeHash :: Fingerprint -> SDoc
pprOnDiskModuleByteCodeHash = entry (text "hash:") . ppr

-- | […]
pprCompiledByteCode :: Module -> CompiledByteCode -> SDoc
pprCompiledByteCode currentModule CompiledByteCode {..}
    = entry (text "compiled bytecode:") $
      vcat [
               pprByteCodeObjects currentModule $ bc_bcos,
               pprDataConstructorInfoTables     $ bc_itbls,
               pprTopLevelStrings               $ bc_strs,
               pprBreakpoints currentModule     $ bc_breaks,
               pprStaticPointerTableEntries     $ bc_spt_entries,
               pprHPCInfo                       $ bc_hpc_info
           ]

-- | […]
pprByteCodeObjects :: Module -> FlatBag UnlinkedBCO -> SDoc
pprByteCodeObjects currentModule = entry (text "bytecode objects:")      .
                                   vcatOrNone                            .
                                   map (pprByteCodeObject currentModule) .
                                   elemsFlatBag

-- | […]
pprByteCodeObject :: Module -> UnlinkedBCO -> SDoc
pprByteCodeObject currentModule byteCodeObject = case byteCodeObject of
    UnlinkedBCO {..}
        -> entry (text "ordinary object" <+> ppr unlinkedBCOName <> text ":") $
           vcat [
                    pprArity                  $ unlinkedBCOArity,
                    pprLiterals currentModule $ unlinkedBCOLits,
                    pprPointers currentModule $ unlinkedBCOPtrs
                ]
    UnlinkedStaticCon {..}
        -> entry (
                     text "static-construction object" <+>
                     ppr unlinkedStaticConName         <>
                     text ":"
                 )
           $
           vcat [
                    pprDataConstructorName    $ unlinkedStaticConDataConName,
                    pprLiftedness             $ not unlinkedStaticConIsUnlifted,
                    pprLiterals currentModule $ unlinkedStaticConLits,
                    pprPointers currentModule $ unlinkedStaticConPtrs
                ]

-- | […]
pprArity :: Int -> SDoc
pprArity = entry (text "arity:") . ppr

-- | […]
pprDataConstructorName :: Name -> SDoc
pprDataConstructorName = entry (text "data constructor name:") . ppr

-- | […]
pprLiftedness :: Bool -> SDoc
pprLiftedness = entry (text "lifted:") . noOrYes

-- | […]
pprLiterals :: Module -> FlatBag BCONPtr -> SDoc
pprLiterals currentModule = entry (text "literals:")       .
                            vcatOrNone                     .
                            map (pprLiteral currentModule) .
                            elemsFlatBag

-- | […]
pprLiteral :: Module -> BCONPtr -> SDoc
pprLiteral currentModule literal = case literal of
    BCONPtrWord word
        -> text "word" <+>
           ppr word
    BCONPtrLbl label
        -> text "label" <+>
           ppr label
    BCONPtrItbl infoTableName
        -> text "info table of" <+>
           ppr infoTableName
    BCONPtrAddr addrName
        -> text "address" <+>
           ppr addrName
    BCONPtrStr encodedString
        -> text "top-level string" <+>
           text (show (utf8DecodeByteString encodedString))
    BCONPtrFS string
        -> text "top-level string" <+>
           text (show (unpackFS string))
    BCONPtrFFIInfo ffiInfo
        -> text "foreign function" <+>
           pprFFIInfo ffiInfo
    BCONPtrCostCentre breakpointID
        -> text "cost center" <+>
           pprInternalBreakpointID currentModule breakpointID

-- | […]
pprFFIInfo :: FFIInfo -> SDoc
pprFFIInfo FFIInfo {..}
    = hsep (map (pprFFIType >>> (<+> text "->")) ffiInfoArgs) <+>
      pprFFIType ffiInfoRet

-- | […]
pprFFIType :: FFIType -> SDoc
pprFFIType ffiType = assert (take 3 ident == "FFI") $ text (drop 3 ident) where

    ident :: String
    ident = show ffiType

-- | […]
pprInternalBreakpointID :: Module -> InternalBreakpointId -> SDoc
pprInternalBreakpointID currentModule InternalBreakpointId {..}
    | ibi_info_mod == currentModule = indexDoc
    | otherwise                     = indexDoc         <+>
                                      text "in"        <+>
                                      ppr ibi_info_mod
    where

    indexDoc :: SDoc
    indexDoc = ppr ibi_info_index

-- | […]
pprPointers :: Module -> FlatBag BCOPtr -> SDoc
pprPointers currentModule = entry (text "pointers:")       .
                            vcatOrNone                     .
                            map (pprPointer currentModule) .
                            elemsFlatBag

-- | […]
pprPointer :: Module -> BCOPtr -> SDoc
pprPointer currentModule pointer = case pointer of
    BCOPtrName name
        -> text "name" <+> ppr name
    BCOPtrPrimOp primOp
        -> text "primitive operation" <+> ppr primOp
    BCOPtrBCO byteCodeObject
        -> pprByteCodeObject currentModule byteCodeObject
    BCOPtrBreakArray breakArrayModule
        -> text "break array of module" <+> ppr breakArrayModule

-- | […]
pprDataConstructorInfoTables :: [(Name, ConInfoTable)] -> SDoc
pprDataConstructorInfoTables = entry (text "data constructor info tables:") .
                               vcatOrNone                                   .
                               map (uncurry pprDataConstructorInfoTable)

-- | […]
pprDataConstructorInfoTable :: Name -> ConInfoTable -> SDoc
pprDataConstructorInfoTable dataConstrName ConInfoTable {..}
    = entry (text "info table of" <+> ppr dataConstrName <> text ":") $
      vcat [
               pprPointerWordCount    $ conItblPtrs,
               pprNonPointerWordCount $ conItblNPtrs
           ]

-- | […]
pprPointerWordCount :: Int -> SDoc
pprPointerWordCount = entry (text "number of words for pointers:") . ppr

-- | […]
pprNonPointerWordCount :: Int -> SDoc
pprNonPointerWordCount = entry (text "number of words for non-pointers:") . ppr

-- | […]
pprTopLevelStrings :: [(Name, ByteString)] -> SDoc
pprTopLevelStrings = entry (text "top-level strings:") .
                     vcatOrNone                        .
                     map (uncurry pprTopLevelString)

-- | […]
pprTopLevelString :: Name -> ByteString -> SDoc
pprTopLevelString stringName encodedString
    = entry (ppr stringName <> text ":") $
      text                               $
      show                               $
      utf8DecodeByteString               $
      encodedString

-- | […]
pprBreakpoints :: Module -> Maybe InternalModBreaks -> SDoc
pprBreakpoints currentModule
    = entry (text "breakpoints:") .
      maybe (text "<none>") (pprBreakpointsData currentModule)

-- | […]
pprBreakpointsData :: Module -> InternalModBreaks -> SDoc
pprBreakpointsData currentModule InternalModBreaks {..}
    = vcat [
               pprBreakpointsInSource currentModule   $ imodBreaks_modBreaks,
               pprBreakpointsInByteCode currentModule $ imodBreaks_breakInfo
           ]

-- | […]
pprBreakpointsInSource :: Module -> ModBreaks -> SDoc
pprBreakpointsInSource currentModule ModBreaks {..}
    = entry (text "breakpoints in source:")                 $
      assert (modBreaks_module == currentModule)            $
      assert boundsAreIdentical                             $
      vcatOrNone                                            $
      pprBreakpointInSource <$> indices modBreaks_locs_ <*>
                                elems modBreaks_locs_   <*>
                                elems modBreaks_decls   <*>
                                elems modBreaks_vars    <*>
                                elems modBreaks_ccs
    where

    boundsAreIdentical :: Bool
    boundsAreIdentical = bounds modBreaks_locs_ == bounds modBreaks_decls &&
                         bounds modBreaks_locs_ == bounds modBreaks_vars  &&
                         bounds modBreaks_locs_ == bounds modBreaks_ccs

-- | […]
pprBreakpointInSource :: BreakTickIndex
                      -> BinSrcSpan
                      -> [String]
                      -> [OccName]
                      -> (ShortByteString, ShortByteString)
                      -> SDoc
pprBreakpointInSource ix srcSpan declarationPath freeVars costCenterInfo
    = entry (text "breakpoint" <+> ppr ix <> text ":") $
      vcat [
               pprSrcSpan            $ srcSpan,
               pprDeclarationPath    $ declarationPath,
               pprFreeVariables      $ freeVars,
               pprCostCenterPath     $ costCenterPath,
               pprCostCenterLocation $ costCenterLocation
           ]
    where

    costCenterPath :: String
    costCenterPath = utf8DecodeShortByteString (fst costCenterInfo)

    costCenterLocation :: String
    costCenterLocation = utf8DecodeShortByteString (snd costCenterInfo)

    -- The structure of the cost center information is apparent from the
    -- implementation of 'GHC.HsToCore.Breakpoints.mkModBreaks'.

-- | […]
pprSrcSpan :: BinSrcSpan -> SDoc
pprSrcSpan = entry (text "source span:") . ppr . unBinSrcSpan

-- | […]
pprDeclarationPath :: [String] -> SDoc
pprDeclarationPath = entry (text "declaration path:") . vcat . map text

-- | […]
pprFreeVariables :: [OccName] -> SDoc
pprFreeVariables = entry (text "free variables:") . hsep . map ppr

-- | […]
pprCostCenterPath :: String -> SDoc
pprCostCenterPath = entry (text "cost center path:") . text

-- | […]
pprCostCenterLocation :: String -> SDoc
pprCostCenterLocation = entry (text "cost center location:") . text

-- | […]
pprBreakpointsInByteCode :: Module -> IntMap CgBreakInfo -> SDoc
pprBreakpointsInByteCode currentModule
    = entry (text "breakpoints in bytecode:")               .
      vcatOrNone                                            .
      map (uncurry (pprBreakpointInByteCode currentModule)) .
      IntMap.toList

-- | […]
pprBreakpointInByteCode :: Module -> Int -> CgBreakInfo -> SDoc
pprBreakpointInByteCode currentModule ix CgBreakInfo {..}
    = entry (text "breakpoint" <+> ppr ix <> text ":") $
      vcat [
               pprType                 $ cgb_resty,
               pprTypeVariables        $ cgb_tyvars,
               pprVariables            $ cgb_vars,
               pprOrigin currentModule $ cgb_tick_id
           ]
    -- That the 'cgb_resty' field holds the type of the breakpoint is apparent
    -- from the fact that this field is set by
    -- 'GHC.StgToByteCode.dehydrateCgBreakInfo' using one of its arguments and
    -- 'dehydrateCgBreakInfo' is always invoked with this argument set to the
    -- extension field of 'Breakpoint', which in turn holds the type of the
    -- breakpoint according to Note [Tickish passes] and the comment on the
    -- instance declaration of @XBreakpoint 'TickishPassStg@.

pprType :: IfaceType -> SDoc
pprType = entry (text "type:") . ppr

-- | […]
pprTypeVariables :: [IfaceTvBndr] -> SDoc
pprTypeVariables = entry (text "type variables:") .
                   vcat                           .
                   map pprTypeVariableBinder

-- | […]
pprTypeVariableBinder :: IfaceTvBndr -> SDoc
pprTypeVariableBinder (name, kind) = ppr name <+> text "::" <+> ppr kind

-- | […]
pprVariables :: [Maybe (IfaceIdBndr, Word)] -> SDoc
pprVariables = entry (text "variables:") .
               vcat                           .
               map pprVariable

-- | […]
pprVariable :: Maybe (IfaceIdBndr, Word) -> SDoc
pprVariable = maybe (text "<unknown>") (uncurry pprKnownVariable)

pprKnownVariable :: IfaceIdBndr -> Word -> SDoc
pprKnownVariable binder offset = pprVariableBinder binder <+>
                                 text "@"                 <+>
                                 ppr offset
-- That the second argument is an offset is apparent from the use of the
-- identifier @offset@ in the implementation of
-- 'GHC.StgToByteCode.dehydrateCgBreakInfo'.

-- | […]
pprVariableBinder :: IfaceIdBndr -> SDoc
pprVariableBinder (multiplicity, name, type_)
    = text "%" <> ppr multiplicity <+>
      ppr name <+> text "::" <+> ppr type_

pprOrigin :: Module -> Either InternalBreakLoc BreakpointId -> SDoc
pprOrigin currentModule = entry (text "origin:")        .
                          pprBreakpointID currentModule .
                          either internalBreakLoc id

-- | […] [analogous to 'pprInternalBreakpointID' but the meaning of the index is different]
pprBreakpointID :: Module -> BreakpointId -> SDoc
pprBreakpointID currentModule BreakpointId {..}
    | bi_tick_mod == currentModule = indexDoc
    | otherwise                    = indexDoc <+> text "in" <+> ppr bi_tick_mod
    where

    indexDoc :: SDoc
    indexDoc = ppr bi_tick_index

-- | […]
pprStaticPointerTableEntries :: [SptEntry] -> SDoc
pprStaticPointerTableEntries = entry (text "static-pointer table entries:") .
                               vcatOrNone                                   .
                               map pprStaticPointerTableEntry

-- | […]
pprStaticPointerTableEntry :: SptEntry -> SDoc
pprStaticPointerTableEntry (SptEntry name fingerprint)
    = ppr fingerprint <> text ":" <+> ppr name

-- | […]
pprHPCInfo :: Strict.Maybe ByteCodeHpcInfo -> SDoc
pprHPCInfo = entry (text "HPC information:") .
             Strict.maybe (text "<none>") pprHPCInfoData

-- | […]
pprHPCInfoData :: ByteCodeHpcInfo -> SDoc
pprHPCInfoData ByteCodeHpcInfo {..}
    = vcat [
               pprHPCInfoHash $ bchi_hash,
               pprModuleName  $ bchi_module_name,
               pprTickBoxName $ bchi_tickbox_name,
               pprTickCount   $ bchi_tick_count
           ]
    where

-- | […]
pprHPCInfoHash :: Int -> SDoc
pprHPCInfoHash = entry (text "hash:") . pprFixedSizeNatural

-- | […]
pprModuleName :: ShortByteString -> SDoc
pprModuleName = entry (text "module name:") .
                text                        .
                utf8DecodeShortByteString

-- | […]
pprTickBoxName :: ShortByteString -> SDoc
pprTickBoxName = entry (text "tick box name:") .
                 text                          .
                 utf8DecodeShortByteString

-- | […]
pprTickCount :: Int -> SDoc
pprTickCount = entry (text "number of ticks:") . ppr

-- | […]
pprObjectFileContents :: [ByteString] -> SDoc
pprObjectFileContents = entry (text "contents of object files:") .
                        vcatOrNone                               .
                        zipWith pprObjectFileContent [0 ..]

-- | […]
pprObjectFileContent :: Int -> ByteString -> SDoc
pprObjectFileContent ix = entry (text "file" <+> ppr ix <> text ":") .
                          pprByteString

-- | […]
pprByteString :: ByteString -> SDoc
pprByteString = pprFixedSizeNaturalList . unpack

-- | […]
pprFixedSizeNaturalList :: (Integral a, FiniteBits a) => [a] -> SDoc
pprFixedSizeNaturalList []   = text "<empty>"
pprFixedSizeNaturalList list = hsep (map pprFixedSizeNatural list)

-- | […]
pprFixedSizeNatural :: (Integral a, FiniteBits a) => a -> SDoc
pprFixedSizeNatural num
    = text $ replicate (digitCount - length unpadded) '0' ++ unpadded
    where

    digitCount :: Int
    digitCount = (finiteBitSize num + 3) `div` 4

    unpadded :: String
    unpadded = showHex num ""

-- | […]
noOrYes :: Bool -> SDoc
noOrYes bool = text (if bool then "yes" else "no")

-- | […]
entry :: SDoc -> SDoc -> SDoc
entry title content = hang title 2 content

-- | […]
vcatOrNone :: [SDoc] -> SDoc
vcatOrNone []   = text "<none>"
vcatOrNone docs = vcat docs
