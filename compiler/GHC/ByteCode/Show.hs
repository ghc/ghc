{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module implements the output of textual information about the contents
--   of bytecode files. It is the backbone of the @--show-byte-code@ option.
module GHC.ByteCode.Show (showByteCode) where

import Prelude ((+), (-), Integral, div)
import Control.Arrow ((>>>))
import Control.Exception (assert)
import Data.Eq ((==))
import Data.Ord ((>=))
import Data.Bits (FiniteBits, finiteBitSize)
import Data.Function (($), id, (.))
import Data.Tuple (fst, uncurry)
import Data.Bool (Bool, otherwise, not)
import Data.Int (Int)
import Data.Word (Word)
import Data.Maybe (Maybe, maybe)
import Data.Either (Either, either)
import Data.List (length, (++), map, zipWith4, take, drop, replicate)
import Data.String (String)
import Data.ByteString (ByteString)
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
           quotes,
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

-- | Outputs textual information about the contents of a bytecode file.
showByteCode :: Logger -> HscEnv -> FilePath -> IO ()
showByteCode logger env path = do
    byteCode <- readOnDiskModuleByteCode env path
    logMsg logger
           MCDump
           noSrcSpan
           (withPprStyle defaultDumpStyle $ pprOnDiskModuleByteCode byteCode)

-- | Constructs textual information about the contents of a bytecode file.
pprOnDiskModuleByteCode :: OnDiskModuleByteCode -> SDoc
pprOnDiskModuleByteCode OnDiskModuleByteCode {..}
    = vcat [
               pprModuleIdent                   $ odgbc_module,
               pprOnDiskModuleByteCodeHash      $ odgbc_hash,
               pprCompiledByteCode odgbc_module $ odgbc_compiled_byte_code
           ]

-- | Constructs textual information about the name of a module.
pprModuleIdent :: Module -> SDoc
pprModuleIdent = entry (text "name") . ppr

-- | Constructs textual information about the hash of a module.
pprOnDiskModuleByteCodeHash :: Fingerprint -> SDoc
pprOnDiskModuleByteCodeHash = entry (text "hash") . ppr

-- | Constructs textual information about bytecode.
pprCompiledByteCode :: Module           -- ^ The enclosing module
                    -> CompiledByteCode -- ^ The bytecode
                    -> SDoc             -- ^ The textual information
pprCompiledByteCode currentModule CompiledByteCode {..}
    = vcat [
               pprByteCodeObjects currentModule $ bc_bcos,
               pprDataConstructorInfoTables     $ bc_itbls,
               pprTopLevelStrings               $ bc_strs,
               pprBreakpoints currentModule     $ bc_breaks,
               pprStaticPointerTableEntries     $ bc_spt_entries,
               pprHPCInfo                       $ bc_hpc_info
           ]

-- | Constructs textual information about bytecode objects.
pprByteCodeObjects :: Module              -- ^ The enlosing module
                   -> FlatBag UnlinkedBCO -- ^ The bytecode objects
                   -> SDoc                -- ^ The textual information
pprByteCodeObjects currentModule = entry (text "objects")                .
                                   vcatOrNone                            .
                                   map (pprByteCodeObject currentModule) .
                                   elemsFlatBag

-- | Constructs textual information about a single bytecode object.
pprByteCodeObject :: Module      -- ^ The enclosing module
                  -> UnlinkedBCO -- ^ The bytecode object
                  -> SDoc        -- ^ The textual information
pprByteCodeObject currentModule byteCodeObject = case byteCodeObject of
    UnlinkedBCO {..}
        -> entry (text "ordinary object" <+> quotes (ppr unlinkedBCOName)) $
           vcat [
                    pprArity                  $ unlinkedBCOArity,
                    pprLiterals currentModule $ unlinkedBCOLits,
                    pprPointers currentModule $ unlinkedBCOPtrs
                ]
    UnlinkedStaticCon {..}
        -> entry (
                     text "static-construction object"  <+>
                     quotes (ppr unlinkedStaticConName)
                 )
           $
           vcat [
                    pprDataConstructorName    $ unlinkedStaticConDataConName,
                    pprLiftedness             $ not unlinkedStaticConIsUnlifted,
                    pprLiterals currentModule $ unlinkedStaticConLits,
                    pprPointers currentModule $ unlinkedStaticConPtrs
                ]

-- | Constructs textual information about the arity of an ordinary bytecode
--   object.
pprArity :: Int -> SDoc
pprArity = entry (text "arity") . ppr

-- | Constructs textual information about the data constructor name of a
--   static-construction bytecode object.
pprDataConstructorName :: Name -> SDoc
pprDataConstructorName = entry (text "data constructor name") . ppr

-- | Constructs textual information about the liftedness of a
--   static-construction bytecode object.
pprLiftedness :: Bool -> SDoc
pprLiftedness = entry (text "lifted") . noOrYes

-- | Constructs textual information about literals.
pprLiterals :: Module          -- ^ The enclosing module
            -> FlatBag BCONPtr -- ^ The literals
            -> SDoc            -- ^ The textual information
pprLiterals currentModule = entry (text "literals")        .
                            vcatOrNone                     .
                            map (pprLiteral currentModule) .
                            elemsFlatBag

-- | Constructs textual information about a single literal.
pprLiteral :: Module  -- ^ The enclosing module
           -> BCONPtr -- ^ The literal
           -> SDoc    -- ^ The textual information
pprLiteral currentModule literal = case literal of
    BCONPtrWord word
        -> text "word" <+>
           ppr word
    BCONPtrLbl label
        -> text "label" <+>
           quotes (ppr label)
    BCONPtrItbl infoTableName
        -> text "info table of" <+>
           quotes (ppr infoTableName)
    BCONPtrAddr addrName
        -> text "address" <+>
           quotes (ppr addrName)
    BCONPtrStr encodedString
        -> text "top-level string" <+>
           text (show (utf8DecodeByteString encodedString))
    BCONPtrFS string
        -> text "top-level string" <+>
           text (show (unpackFS string))
    BCONPtrFFIInfo ffiInfo
        -> text "foreign function" <+>
           quotes (pprFFIInfo ffiInfo)
    BCONPtrCostCentre breakpointID
        -> text "cost center of breakpoint" <+>
           pprInternalBreakpointID currentModule breakpointID

-- | Constructs textual information about FFI info.
pprFFIInfo :: FFIInfo -> SDoc
pprFFIInfo FFIInfo {..}
    = hsep (map (pprFFIType >>> (<+> text "->")) ffiInfoArgs) <+>
      pprFFIType ffiInfoRet

-- | Constructs textual information about an FFI type.
pprFFIType :: FFIType -> SDoc
pprFFIType ffiType = assert (take 3 ident == "FFI") $ text (drop 3 ident) where

    ident :: String
    ident = show ffiType

-- | Constructs textual information about the ID of a bytecode breakpoint.
pprInternalBreakpointID
    :: Module               -- ^ The enclosing module
    -> InternalBreakpointId -- ^ The ID of the bytecode breakpoint
    -> SDoc                 -- ^ The textual information
pprInternalBreakpointID currentModule InternalBreakpointId {..}
    | ibi_info_mod == currentModule = indexDoc
    | otherwise                     = indexDoc         <+>
                                      text "in"        <+>
                                      ppr ibi_info_mod
    where

    indexDoc :: SDoc
    indexDoc = ppr ibi_info_index

-- | Constructs textual information about pointers.
pprPointers :: Module         -- ^ The enclosing module
            -> FlatBag BCOPtr -- ^ The pointers
            -> SDoc           -- ^ The textual information
pprPointers currentModule = entry (text "utilized items")  .
                            vcatOrNone                     .
                            map (pprPointer currentModule) .
                            elemsFlatBag

-- | Constructs textual information about a single pointer.
pprPointer :: Module -- ^ The enclosing module
           -> BCOPtr -- ^ The pointer
           -> SDoc   -- ^ The textual information
pprPointer currentModule pointer = case pointer of
    BCOPtrName name
        -> text "item named" <+> quotes (ppr name)
    BCOPtrPrimOp primOp
        -> text "primitive operation" <+> quotes (ppr primOp)
    BCOPtrBCO byteCodeObject
        -> pprByteCodeObject currentModule byteCodeObject
    BCOPtrBreakArray breakArrayModule
        -> text "break array of module" <+> quotes (ppr breakArrayModule)

-- | Constructs textual information about data constructor info tables.
pprDataConstructorInfoTables :: [(Name, ConInfoTable)] -> SDoc
pprDataConstructorInfoTables = entry (text "data constructor info tables") .
                               vcatOrNone                                  .
                               map (uncurry pprDataConstructorInfoTable)

-- | Constructs textual information about a single data constructor info table.
pprDataConstructorInfoTable :: Name -> ConInfoTable -> SDoc
pprDataConstructorInfoTable dataConstrName ConInfoTable {..}
    = entry (text "info table of" <+> quotes (ppr dataConstrName)) $
      vcat [
               pprPointerWordCount    $ conItblPtrs,
               pprNonPointerWordCount $ conItblNPtrs
           ]

-- | Constructs textual information about a number of pointer words.
pprPointerWordCount :: Int -> SDoc
pprPointerWordCount = entry (text "number of words for pointers") . ppr

-- | Constructs textual information about a number of non-pointer words.
pprNonPointerWordCount :: Int -> SDoc
pprNonPointerWordCount = entry (text "number of words for non-pointers") . ppr

-- | Constructs textual information about top-level strings.
pprTopLevelStrings :: [(Name, ByteString)] -> SDoc
pprTopLevelStrings = entry (text "top-level strings") .
                     vcatOrNone                       .
                     map (uncurry pprTopLevelString)

-- | Constructs textual information about a single top-level string.
pprTopLevelString :: Name -> ByteString -> SDoc
pprTopLevelString stringName encodedString = entry (ppr stringName) $
                                             text                   $
                                             show                   $
                                             utf8DecodeByteString   $
                                             encodedString

-- | Constructs textual information about breakpoints.
pprBreakpoints :: Module                  -- ^ The enclosing module
               -> Maybe InternalModBreaks -- ^ The breakpoints
               -> SDoc                    -- ^ The textual information
pprBreakpoints currentModule
    = entry (text "breakpoints") .
      maybe (text "<none>") (pprActualBreakpoints currentModule)

-- | Constructs textual information about actual breakpoints.
pprActualBreakpoints :: Module            -- ^ The enclosing module
                     -> InternalModBreaks -- ^ The actual breakpoints
                     -> SDoc              -- ^ The textual information
pprActualBreakpoints currentModule InternalModBreaks {..}
    = vcat [
               pprSourceBreakpoints currentModule   $ imodBreaks_modBreaks,
               pprByteCodeBreakpoints currentModule $ imodBreaks_breakInfo
           ]

-- | Constructs textual information about source breakpoints.
pprSourceBreakpoints :: Module    -- ^ The enclosing module
                     -> ModBreaks -- ^ The source breakpoints
                     -> SDoc      -- ^ The textual information
pprSourceBreakpoints currentModule ModBreaks {..}
    = entry (text "source breakpoints")                         $
      assert (modBreaks_module == currentModule)                $
      assert (bounds modBreaks_locs_ == bounds modBreaks_decls) $
      assert (bounds modBreaks_locs_ == bounds modBreaks_vars)  $
      vcatOrNone                                                $
      zipWith4 pprSourceBreakpoint (indices modBreaks_locs_)
                                   (elems modBreaks_locs_)
                                   (elems modBreaks_decls)
                                   (elems modBreaks_vars)
      -- The cost center infos in 'modBreaks_ccs', when present, just contain
      -- textual representations of the declaration paths in 'modBreaks_decls'
      -- and the source spans in 'modBreaks_locs_' and are therefore never
      -- shown.

-- | Constructs textual information about a single source breakpoint.
pprSourceBreakpoint :: BreakTickIndex
                    -> BinSrcSpan
                    -> [String]
                    -> [OccName]
                    -> SDoc
pprSourceBreakpoint ix srcSpan declarationPath freeVars
    = entry (text "source breakpoint" <+> ppr ix) $
      vcat [
               pprSrcSpan         $ srcSpan,
               pprDeclarationPath $ declarationPath,
               pprFreeVariables   $ freeVars
           ]

-- | Constructs textual information about a source span.
pprSrcSpan :: BinSrcSpan -> SDoc
pprSrcSpan = entry (text "source span") . ppr . unBinSrcSpan

-- | Constructs textual information about a declaration path.
pprDeclarationPath :: [String] -> SDoc
pprDeclarationPath = entry (text "declaration path") . vcatOrEmpty . map text

-- | Constructs textual information about free variables.
pprFreeVariables :: [OccName] -> SDoc
pprFreeVariables = entry (text "free variables") . vcatOrNone . map ppr

-- | Constructs textual information about bytecode breakpoints.
pprByteCodeBreakpoints :: Module             -- ^ The enclosing module
                       -> IntMap CgBreakInfo -- ^ The bytecode breakpoints
                       -> SDoc               -- ^ The textual information
pprByteCodeBreakpoints currentModule
    = entry (text "bytecode breakpoints")                 .
      vcatOrNone                                          .
      map (uncurry (pprByteCodeBreakpoint currentModule)) .
      IntMap.toList

-- | Constructs textual information about a single bytecode breakpoint.
pprByteCodeBreakpoint :: Module      -- ^ The enclosing module
                      -> Int         -- ^ The index of the bytecode breakpoint
                      -> CgBreakInfo -- ^ The bytecode breakpoint
                      -> SDoc        -- ^ The textual information
pprByteCodeBreakpoint currentModule ix CgBreakInfo {..}
    = entry (text "bytecode breakpoint" <+> ppr ix) $
      vcat [
               pprType                                        $ cgb_resty,
               pprTypeVariables                               $ cgb_tyvars,
               pprVariables                                   $ cgb_vars,
               pprCorrespondingSourceBreakpoint currentModule $ cgb_tick_id
           ]
    -- That the 'cgb_resty' field holds the type of the breakpoint is apparent
    -- from the fact that this field is set by
    -- 'GHC.StgToByteCode.dehydrateCgBreakInfo' using one of its arguments and
    -- 'GHC.StgToByteCode.dehydrateCgBreakInfo' is always invoked with this
    -- argument set to the extension field of 'Breakpoint', which in turn holds
    -- the type of the breakpoint according to Note [Tickish passes] and the
    -- comment on the instance declaration of @XBreakpoint 'TickishPassStg@.

-- | Constructs textual information about a type.
pprType :: IfaceType -> SDoc
pprType = entry (text "type") . ppr

-- | Constructs textual information about type variables.
pprTypeVariables :: [IfaceTvBndr] -> SDoc
pprTypeVariables = entry (text "type variables") .
                   vcatOrNone                    .
                   map pprTypeVariableBinder

-- | Constructs textual information about a type variable binder.
pprTypeVariableBinder :: IfaceTvBndr -> SDoc
pprTypeVariableBinder (name, kind) = ppr name <+> text "::" <+> ppr kind

-- | Constructs textual information about variables.
pprVariables :: [Maybe (IfaceIdBndr, Word)] -> SDoc
pprVariables = entry (text "variables") . vcatOrNone . map pprVariable

-- | Constructs textual information about a single variable.
pprVariable :: Maybe (IfaceIdBndr, Word) -> SDoc
pprVariable = maybe (text "<unknown>") (pprVariableBinder . fst)

-- | Constructs textual information about a variable binder.
pprVariableBinder :: IfaceIdBndr -> SDoc
pprVariableBinder (multiplicity, name, type_)
    = text "%" <> ppr multiplicity <+>
      ppr name <+> text "::" <+> ppr type_

-- | Constructs textual information about a source breakpoint corresponding to a
--   bytecode breakpoint.
pprCorrespondingSourceBreakpoint :: Module
                                    -- ^ The enclosing module
                                 -> Either InternalBreakLoc BreakpointId
                                    -- ^ A reference to the source breakpoint
                                 -> SDoc
                                    -- ^ The textual information
pprCorrespondingSourceBreakpoint currentModule
    = entry (text "corresponding source breakpoint") .
      pprBreakpointID currentModule                  .
      either internalBreakLoc id

-- | Constructs textual information about the ID of a source breakpoint.
pprBreakpointID :: Module       -- ^ The enclosing module
                -> BreakpointId -- ^ The ID of the source breakpoint
                -> SDoc         -- ^ The textual information
pprBreakpointID currentModule BreakpointId {..}
    | bi_tick_mod == currentModule = indexDoc
    | otherwise                    = indexDoc                 <+>
                                     text "in"                <+>
                                     quotes (ppr bi_tick_mod)
    where

    indexDoc :: SDoc
    indexDoc = ppr bi_tick_index

-- | Constructs textual information about static-pointer table entries.
pprStaticPointerTableEntries :: [SptEntry] -> SDoc
pprStaticPointerTableEntries = entry (text "static-pointer table entries") .
                               vcatOrNone                                  .
                               map pprStaticPointerTableEntry

-- | Constructs textual information about a single static-pointer table entry.
pprStaticPointerTableEntry :: SptEntry -> SDoc
pprStaticPointerTableEntry (SptEntry name fingerprint)
    = ppr fingerprint <> text ":" <+> ppr name

-- | Constructs textual information about HPC info.
pprHPCInfo :: Strict.Maybe ByteCodeHpcInfo -> SDoc
pprHPCInfo = entry (text "HPC information") .
             Strict.maybe (text "<none>") pprActualHPCInfo

-- | Constructs textual information about actual HPC info.
pprActualHPCInfo :: ByteCodeHpcInfo -> SDoc
pprActualHPCInfo ByteCodeHpcInfo {..}
    = vcat [
               pprHPCInfoHash $ bchi_hash,
               pprModuleName  $ bchi_module_name,
               pprTickBoxName $ bchi_tickbox_name,
               pprTickCount   $ bchi_tick_count
           ]
    where

-- | Constructs textual information about the hash of HPC info.
pprHPCInfoHash :: Int -> SDoc
pprHPCInfoHash = entry (text "hash") . pprFixedSizeNatural

-- | Constructs textual information about a module name.
pprModuleName :: ShortByteString -> SDoc
pprModuleName = entry (text "module name") .
                text                       .
                utf8DecodeShortByteString

-- | Constructs textual information about a tick box name.
pprTickBoxName :: ShortByteString -> SDoc
pprTickBoxName = entry (text "tick box name") .
                 text                         .
                 utf8DecodeShortByteString

-- | Constructs textual information about a number of tick counts.
pprTickCount :: Int -> SDoc
pprTickCount = entry (text "number of ticks") . ppr

-- | Constructs a hexadecimal representation of a natural number such that the
--   number of hexadecimal digits fits the number of bits used to represent the
--   natural number.
pprFixedSizeNatural :: (Integral a, FiniteBits a) => a -> SDoc
pprFixedSizeNatural num
    = assert (num >= 0) $
      text $ replicate (digitCount - length unpadded) '0' ++ unpadded
    where

    digitCount :: Int
    digitCount = (finiteBitSize num + 3) `div` 4

    unpadded :: String
    unpadded = showHex num ""

-- | Constructs a textual representation of a boolean, interpreting 'True' and
--   'False' as “yes” and “no”, respectively.
noOrYes :: Bool -> SDoc
noOrYes bool = text (if bool then "yes" else "no")

-- | Constructs an entry in a list of textual data representations.
entry :: SDoc -- ^ The title of the entry
      -> SDoc -- ^ The contents of the entry
      -> SDoc -- ^ The entry
entry title content = hang (title <> text ":") 2 content

-- | Composes documents vertically in general, but presents an empty document
--   list as `<none`>.
vcatOrNone :: [SDoc] -> SDoc
vcatOrNone []   = text "<none>"
vcatOrNone docs = vcat docs

-- | Composes documents vertically in general, but presents an empty document
--   list as `<empty`>.
vcatOrEmpty :: [SDoc] -> SDoc
vcatOrEmpty []   = text "<empty>"
vcatOrEmpty docs = vcat docs
