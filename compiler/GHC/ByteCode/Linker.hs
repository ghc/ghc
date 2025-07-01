{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DataKinds             #-}
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
  )
where

import GHC.Prelude

import GHC.Runtime.Interpreter
import GHC.ByteCode.Types
import GHCi.RemoteTypes
import GHCi.ResolvedBCO

import GHC.Builtin.PrimOps
import GHC.Builtin.PrimOps.Ids

import GHC.Unit.Module.Env
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

{-
  Linking interpretables into something we can run
-}

linkBCO
  :: Interp
  -> PkgsLoaded
  -> LinkerEnv
  -> NameEnv Int
  -> UnlinkedBCO
  -> IO ResolvedBCO
linkBCO interp pkgs_loaded le bco_ix
           (UnlinkedBCO _ arity insns bitmap lits0 ptrs0) = do
  -- fromIntegral Word -> Word64 should be a no op if Word is Word64
  -- otherwise it will result in a cast to longlong on 32bit systems.
  (lits :: [Word]) <- mapM (fmap fromIntegral . lookupLiteral interp pkgs_loaded le) (elemsFlatBag lits0)
  ptrs <- mapM (resolvePtr interp pkgs_loaded le bco_ix) (elemsFlatBag ptrs0)
  let lits' = listArray (0 :: Int, fromIntegral (sizeFlatBag lits0)-1) lits
  return $ ResolvedBCO { resolvedBCOIsLE   = isLittleEndian
                       , resolvedBCOArity  = arity
                       , resolvedBCOInstrs = insns
                       , resolvedBCOBitmap = bitmap
                       , resolvedBCOLits   = mkBCOByteArray lits'
                       , resolvedBCOPtrs   = addListToSS emptySS ptrs
                       }

lookupLiteral :: Interp -> PkgsLoaded -> LinkerEnv -> BCONPtr -> IO Word
lookupLiteral interp pkgs_loaded le ptr = case ptr of
  BCONPtrWord lit -> return lit
  BCONPtrLbl  sym -> do
    Ptr a# <- lookupStaticPtr interp sym
    return (W# (int2Word# (addr2Int# a#)))
  BCONPtrItbl nm -> do
    Ptr a# <- lookupIE interp pkgs_loaded (itbl_env le) nm
    return (W# (int2Word# (addr2Int# a#)))
  BCONPtrAddr nm -> do
    Ptr a# <- lookupAddr interp pkgs_loaded (addr_env le) nm
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
  BCONPtrCostCentre BreakpointId{..}
    | interpreterProfiled interp -> do
        case expectJust (lookupModuleEnv (ccs_env le) bi_tick_mod) ! bi_tick_index of
          RemotePtr p -> pure $ fromIntegral p
    | otherwise ->
        case toRemotePtr nullPtr of
          RemotePtr p -> pure $ fromIntegral p

lookupStaticPtr :: Interp -> FastString -> IO (Ptr ())
lookupStaticPtr interp addr_of_label_string = do
  m <- lookupSymbol interp (IFaststringSymbol addr_of_label_string)
  case m of
    Just ptr -> return ptr
    Nothing  -> linkFail "GHC.ByteCode.Linker: can't find label"
                  (ppr addr_of_label_string)

lookupIE :: Interp -> PkgsLoaded -> ItblEnv -> Name -> IO (Ptr ())
lookupIE interp pkgs_loaded ie con_nm =
  case lookupNameEnv ie con_nm of
    Just (_, ItblPtr a) -> return (fromRemotePtr (castRemotePtr a))
    Nothing -> do -- try looking up in the object files.
       let sym_to_find1 = IConInfoSymbol con_nm
       m <- lookupHsSymbol interp pkgs_loaded sym_to_find1
       case m of
          Just addr -> return addr
          Nothing
             -> do -- perhaps a nullary constructor?
                   let sym_to_find2 = IStaticInfoSymbol con_nm
                   n <- lookupHsSymbol interp pkgs_loaded sym_to_find2
                   case n of
                      Just addr -> return addr
                      Nothing   -> linkFail "GHC.ByteCode.Linker.lookupIE"
                                      (ppr sym_to_find1 <> " or " <>
                                       ppr sym_to_find2)

-- see Note [Generating code for top-level string literal bindings] in GHC.StgToByteCode
lookupAddr :: Interp -> PkgsLoaded -> AddrEnv -> Name -> IO (Ptr ())
lookupAddr interp pkgs_loaded ae addr_nm = do
  case lookupNameEnv ae addr_nm of
    Just (_, AddrPtr ptr) -> return (fromRemotePtr ptr)
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
    Just p -> return (toRemotePtr p)
    Nothing -> linkFail "GHC.ByteCode.Linker.lookupCE(primop)" (text sym_to_find)

resolvePtr
  :: Interp
  -> PkgsLoaded
  -> LinkerEnv
  -> NameEnv Int
  -> BCOPtr
  -> IO ResolvedBCOPtr
resolvePtr interp pkgs_loaded le bco_ix ptr = case ptr of
  BCOPtrName nm
    | Just ix <- lookupNameEnv bco_ix nm
    -> return (ResolvedBCORef ix) -- ref to another BCO in this group

    | Just (_, rhv) <- lookupNameEnv (closure_env le) nm
    -> return (ResolvedBCOPtr (unsafeForeignRefToRemoteRef rhv))

    | otherwise
    -> assertPpr (isExternalName nm) (ppr nm) $
       do
          let sym_to_find = IClosureSymbol nm
          m <- lookupHsSymbol interp pkgs_loaded sym_to_find
          case m of
            Just p -> return (ResolvedBCOStaticPtr (toRemotePtr p))
            Nothing -> linkFail "GHC.ByteCode.Linker.lookupCE" (ppr sym_to_find)

  BCOPtrPrimOp op
    -> ResolvedBCOStaticPtr <$> lookupPrimOp interp pkgs_loaded op

  BCOPtrBCO bco
    -> ResolvedBCOPtrBCO <$> linkBCO interp pkgs_loaded le bco_ix bco

  BCOPtrBreakArray tick_mod ->
    withForeignRef (expectJust (lookupModuleEnv (breakarray_env le) tick_mod)) $
      \ba -> pure $ ResolvedBCOPtrBreakArray ba

-- | Look up the address of a Haskell symbol in the currently
-- loaded units.
--
-- See Note [Looking up symbols in the relevant objects].
lookupHsSymbol :: Interp -> PkgsLoaded -> InterpSymbol (Suffix s) -> IO (Maybe (Ptr ()))
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
