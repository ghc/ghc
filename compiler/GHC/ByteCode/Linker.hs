{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -optc-DNON_POSIX_SOURCE #-}
--
--  (c) The University of Glasgow 2002-2006
--

-- | Bytecode assembler and linker
module GHC.ByteCode.Linker
  ( linkBCO
  , lookupStaticPtr
  , lookupIE
  , linkFail
  , BCOIx(..)
  )
where

import GHC.Prelude

import GHC.Runtime.Interpreter
import GHC.ByteCode.Types
import GHCi.RemoteTypes
import GHCi.ResolvedBCO

import GHC.Builtin.PrimOps
import GHC.Builtin.PrimOps.Ids

import GHC.Unit.Types

import GHC.Data.FastString
import GHC.Data.Maybe
import GHC.Data.SizedSeq

import GHC.Linker.Types

import GHC.Utils.Panic
import GHC.Utils.Outputable

import GHC.Types.Name
import GHC.Types.Name.Env
import qualified GHC.Types.Id as Id
import GHC.Types.Unique.DFM

-- Standard libraries
import Data.Array.Unboxed
import Foreign.Ptr
import GHC.Exts
import qualified GHC.Exts.Heap as Heap

{- |
  Linking interpretables into something we can run
-}
linkBCO
  :: Interp
  -> PkgsLoaded
  -> BytecodeLoaderState
  -> NameEnv BCOIx
  -- ^ A mapping from names to references to other BCOs
  --   or static constructors in this group.
  -> UnlinkedBCO
  -> IO ResolvedBCO
linkBCO interp pkgs_loaded bytecode_state bco_ix unl_bco = do
  case unl_bco of
    UnlinkedBCO _ arity insns
           bitmap lits0 ptrs0 -> do
        lits <- doLits lits0
        ptrs <- doPtrs ptrs0
        return ResolvedBCO
          { resolvedBCOIsLE   = isLittleEndian
          , resolvedBCOArity  = arity
          , resolvedBCOInstrs = insns
          , resolvedBCOBitmap = bitmap
          , resolvedBCOLits   = lits
          , resolvedBCOPtrs   = ptrs
          }

    UnlinkedStaticCon
      { unlinkedStaticConLits = lits0
      , unlinkedStaticConPtrs = ptrs0
      , unlinkedStaticConDataConName
      , unlinkedStaticConIsUnlifted
      } -> do
        itbl_ptr <- lookupIE interp pkgs_loaded bytecode_state unlinkedStaticConDataConName
        lits <- doLits lits0
        ptrs <- doPtrs ptrs0
        return ResolvedStaticCon
          { resolvedBCOIsLE = isLittleEndian
          , resolvedStaticConInfoPtr = itbl_ptr
          , resolvedStaticConArity = sizeFlatBag lits0 + sizeFlatBag ptrs0 -- ARITY IS WRONG, doesn't account for packing of literals
          , resolvedStaticConLits = lits
          , resolvedStaticConPtrs = ptrs
          , resolvedStaticConIsUnlifted = unlinkedStaticConIsUnlifted
          }
  where
    doLits lits0 = do
      (lits :: [Word]) <- mapM (lookupLiteral interp pkgs_loaded bytecode_state) (elemsFlatBag lits0)
      let lits' = listArray (0 :: Int, fromIntegral (sizeFlatBag lits0)-1) lits -- WRONG, doesn't account for packing.
      return $ mkBCOByteArray lits'
    doPtrs ptrs0 = addListToSS emptySS <$> do
      mapM (resolvePtr interp pkgs_loaded bytecode_state bco_ix) (elemsFlatBag ptrs0)

-- | An index into a BCO or Static Constructor in this group.
--
-- We distinguish between lifted and unlifted static constructors because
-- lifted ones get resolved by tying a knot, since there may be circular
-- dependencies between them, whereas unlifted ones get constructed in a first
-- pass.
data BCOIx = BCOIx !Int
           | LiftedStaticConIx !Int
           | UnliftedStaticConIx !Int
  deriving (Eq, Ord, Show)

lookupLiteral :: Interp -> PkgsLoaded -> BytecodeLoaderState -> BCONPtr -> IO Word
lookupLiteral interp pkgs_loaded bytecode_state ptr = case ptr of
  BCONPtrWord lit -> return lit
  BCONPtrLbl  sym -> do
    Ptr a# <- fromRemotePtr <$> lookupStaticPtr interp sym
    return (W# (int2Word# (addr2Int# a#)))
  BCONPtrItbl nm -> do
    (Ptr a#) <- fromRemotePtr <$> lookupIE interp pkgs_loaded bytecode_state nm
    return (W# (int2Word# (addr2Int# a#)))
  BCONPtrAddr nm -> do
    Ptr a# <- fromRemotePtr <$> lookupAddr interp pkgs_loaded bytecode_state nm
    return (W# (int2Word# (addr2Int# a#)))
  BCONPtrStr bs -> do
    RemotePtr p <- fmap head $ interpCmd interp $ MallocStrings [bs]
    pure $ fromIntegral p
  BCONPtrFS fs -> do
    RemotePtr p <- fmap head $ interpCmd interp $ MallocStrings [bytesFS fs]
    pure $ fromIntegral p
  BCONPtrFFIInfo (FFIInfo {..}) -> do
    RemotePtr p <- interpCmd interp $ PrepFFI ffiInfoArgs ffiInfoRet
    pure $ fromIntegral p
  BCONPtrCostCentre InternalBreakpointId{..}
    | interpreterProfiled interp -> do
        case expectJust (lookupCCSBytecodeState bytecode_state ibi_info_mod) ! ibi_info_index of
          RemotePtr p -> pure $ fromIntegral p
    | otherwise ->
        case toRemotePtr nullPtr of
          RemotePtr p -> pure $ fromIntegral p

lookupStaticPtr :: Interp -> FastString -> IO (RemotePtr ())
lookupStaticPtr interp addr_of_label_string = do
  m <- lookupSymbol interp (IFaststringSymbol addr_of_label_string)
  case m of
    Just ptr -> return ptr
    Nothing  -> linkFail "GHC.ByteCode.Linker: can't find label"
                  (ppr addr_of_label_string)

lookupIE :: Interp -> PkgsLoaded -> BytecodeLoaderState -> Name -> IO (RemotePtr Heap.StgInfoTable)
lookupIE interp pkgs_loaded bytecode_state con_nm =
  case lookupInfoTableBytecodeState bytecode_state con_nm of
    Just (_, ItblPtr a) -> return a
    Nothing -> do -- try looking up in the object files.
       let sym_to_find1 = IConInfoSymbol con_nm
       m <- lookupHsSymbol interp pkgs_loaded sym_to_find1
       case m of
          Just addr -> return (castRemotePtr addr)
          Nothing
             -> do -- perhaps a nullary constructor?
                   let sym_to_find2 = IStaticInfoSymbol con_nm
                   n <- lookupHsSymbol interp pkgs_loaded sym_to_find2
                   case n of
                      Just addr -> return (castRemotePtr addr)
                      Nothing   -> linkFail "GHC.ByteCode.Linker.lookupIE"
                                      (ppr sym_to_find1 <> " or " <>
                                       ppr sym_to_find2)

-- see Note [Generating code for top-level string literal bindings] in GHC.StgToByteCode
lookupAddr :: Interp -> PkgsLoaded -> BytecodeLoaderState -> Name -> IO (RemotePtr ())
lookupAddr interp pkgs_loaded bytecode_state addr_nm = do
  case lookupAddressBytecodeState bytecode_state addr_nm of
    Just (_, AddrPtr ptr) -> return ptr
    Nothing -> do -- try looking up in the object files.
      let sym_to_find = IBytesSymbol addr_nm
                          -- see Note [Bytes label] in GHC.Cmm.CLabel
      m <- lookupHsSymbol interp pkgs_loaded sym_to_find
      case m of
        Just ptr -> return ptr
        Nothing -> linkFail "GHC.ByteCode.Linker.lookupAddr"
                     (ppr sym_to_find)

lookupPrimOp :: Interp -> PkgsLoaded -> PrimOp -> IO (RemotePtr ())
lookupPrimOp interp pkgs_loaded primop = do
  let sym_to_find = primopToCLabel primop "closure"
  m <- lookupHsSymbol interp pkgs_loaded (IClosureSymbol (Id.idName $ primOpId primop))
  case m of
    Just p -> return p
    Nothing -> linkFail "GHC.ByteCode.Linker.lookupCE(primop)" (text sym_to_find)

resolvePtr
  :: Interp
  -> PkgsLoaded
  -> BytecodeLoaderState
  -> NameEnv BCOIx
  -> BCOPtr
  -> IO ResolvedBCOPtr
resolvePtr interp pkgs_loaded bco_loader_state bco_ix ptr = case ptr of
  BCOPtrName nm
    | Just bix <- lookupNameEnv bco_ix nm
    -> return $ case bix of
        BCOIx ix               -> ResolvedBCORef ix
        LiftedStaticConIx ix   -> ResolvedStaticConRef ix
        UnliftedStaticConIx ix -> ResolvedUnliftedStaticConRef ix

    | Just (_, rhv) <- lookupNameBytecodeState bco_loader_state nm
    -> return (ResolvedBCOPtr (unsafeForeignRefToRemoteRef rhv))

    | otherwise
    -> assertPpr (isExternalName nm) (ppr nm) $
       do
          let sym_to_find = IClosureSymbol nm
          m <- lookupHsSymbol interp pkgs_loaded sym_to_find
          case m of
            Just p -> return (ResolvedBCOStaticPtr p)
            Nothing -> linkFail "GHC.ByteCode.Linker.lookupCE" (ppr sym_to_find)

  BCOPtrPrimOp op
    -> ResolvedBCOStaticPtr <$> lookupPrimOp interp pkgs_loaded op

  BCOPtrBCO bco
    -> ResolvedBCOPtrBCO <$> linkBCO interp pkgs_loaded bco_loader_state bco_ix bco

  BCOPtrBreakArray tick_mod ->
    withForeignRef (expectJust (lookupBreakArrayBytecodeState bco_loader_state tick_mod)) $
      \ba -> pure $ ResolvedBCOPtrBreakArray ba

-- | Look up the address of a Haskell symbol in the currently
-- loaded units.
--
-- See Note [Looking up symbols in the relevant objects].
lookupHsSymbol :: Interp -> PkgsLoaded -> InterpSymbol (Suffix s) -> IO (Maybe (RemotePtr ()))
lookupHsSymbol interp pkgs_loaded sym_to_find = do
  massertPpr (isExternalName (interpSymbolName sym_to_find)) (ppr sym_to_find)
  let pkg_id = moduleUnitId $ nameModule (interpSymbolName sym_to_find)
      loaded_dlls = maybe [] loaded_pkg_hs_dlls $ lookupUDFM pkgs_loaded pkg_id

      go (dll:dlls) = do
        mb_ptr <- lookupSymbolInDLL interp dll sym_to_find
        case mb_ptr of
          Just ptr -> pure (Just ptr)
          Nothing -> go dlls
      go [] =
        -- See Note [Symbols may not be found in pkgs_loaded] in GHC.Linker.Types
        lookupSymbol interp sym_to_find

  go loaded_dlls

linkFail :: String -> SDoc -> IO a
linkFail who what
   = throwGhcExceptionIO (ProgramError $
        unlines [ "",who
                , "During interactive linking, GHCi couldn't find the following symbol:"
                , ' ' : ' ' : showSDocUnsafe what
                , "This may be due to you not asking GHCi to load extra object files,"
                , "archives or DLLs needed by your current session.  Restart GHCi, specifying"
                , "the missing library using the -L/path/to/object/dir and -lmissinglibname"
                , "flags, or simply by naming the relevant files on the GHCi command line."
                , "Alternatively, this link failure might indicate a bug in GHCi."
                , "If you suspect the latter, please report this as a GHC bug:"
                , "  https://www.haskell.org/ghc/reportabug"
                ])






-- See Note [Primop wrappers] in GHC.Builtin.PrimOps
primopToCLabel :: PrimOp -> String -> String
primopToCLabel primop suffix = concat
    [ "ghczminternal_GHCziInternalziPrimopWrappers_"
    , zString (zEncodeFS (occNameFS (primOpOcc primop)))
    , '_':suffix
    ]
