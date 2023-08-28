{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -optc-DNON_POSIX_SOURCE #-}
--
--  (c) The University of Glasgow 2002-2006
--

-- | Bytecode assembler and linker
module GHC.ByteCode.Linker
  ( linkBCO
  , lookupStaticPtr
  , lookupIE
  , nameToCLabel
  , linkFail
  )
where

import GHC.Prelude

import GHC.Runtime.Interpreter
import GHC.ByteCode.Types
import GHCi.RemoteTypes
import GHCi.ResolvedBCO
import GHCi.BreakArray

import GHC.Builtin.PrimOps
import GHC.Builtin.PrimOps.Ids
import GHC.Builtin.Names

import GHC.Unit.Types

import GHC.Data.FastString
import GHC.Data.SizedSeq

import GHC.Linker.Types

import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.Outputable

import GHC.Types.Name
import GHC.Types.Name.Env
import qualified GHC.Types.Id as Id
import GHC.Types.Unique.DFM

import Language.Haskell.Syntax.Module.Name

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
  -> RemoteRef BreakArray
  -> UnlinkedBCO
  -> IO ResolvedBCO
linkBCO interp pkgs_loaded le bco_ix breakarray
           (UnlinkedBCO _ arity insns bitmap lits0 ptrs0) = do
  -- fromIntegral Word -> Word64 should be a no op if Word is Word64
  -- otherwise it will result in a cast to longlong on 32bit systems.
  lits <- mapM (fmap fromIntegral . lookupLiteral interp pkgs_loaded le) (ssElts lits0)
  ptrs <- mapM (resolvePtr interp pkgs_loaded le bco_ix breakarray) (ssElts ptrs0)
  return (ResolvedBCO isLittleEndian arity insns bitmap
              (listArray (0, fromIntegral (sizeSS lits0)-1) lits)
              (addListToSS emptySS ptrs))

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
  BCONPtrStr _ ->
    -- should be eliminated during assembleBCOs
    panic "lookupLiteral: BCONPtrStr"

lookupStaticPtr :: Interp -> FastString -> IO (Ptr ())
lookupStaticPtr interp addr_of_label_string = do
  m <- lookupSymbol interp addr_of_label_string
  case m of
    Just ptr -> return ptr
    Nothing  -> linkFail "GHC.ByteCode.Linker: can't find label"
                  (unpackFS addr_of_label_string)

lookupIE :: Interp -> PkgsLoaded -> ItblEnv -> Name -> IO (Ptr ())
lookupIE interp pkgs_loaded ie con_nm =
  case lookupNameEnv ie con_nm of
    Just (_, ItblPtr a) -> return (fromRemotePtr (castRemotePtr a))
    Nothing -> do -- try looking up in the object files.
       let sym_to_find1 = nameToCLabel con_nm "con_info"
       m <- lookupHsSymbol interp pkgs_loaded con_nm "con_info"
       case m of
          Just addr -> return addr
          Nothing
             -> do -- perhaps a nullary constructor?
                   let sym_to_find2 = nameToCLabel con_nm "static_info"
                   n <- lookupHsSymbol interp pkgs_loaded con_nm "static_info"
                   case n of
                      Just addr -> return addr
                      Nothing   -> linkFail "GHC.ByteCode.Linker.lookupIE"
                                      (unpackFS sym_to_find1 ++ " or " ++
                                       unpackFS sym_to_find2)

-- see Note [Generating code for top-level string literal bindings] in GHC.StgToByteCode
lookupAddr :: Interp -> PkgsLoaded -> AddrEnv -> Name -> IO (Ptr ())
lookupAddr interp pkgs_loaded ae addr_nm = do
  case lookupNameEnv ae addr_nm of
    Just (_, AddrPtr ptr) -> return (fromRemotePtr ptr)
    Nothing -> do -- try looking up in the object files.
      let sym_to_find = nameToCLabel addr_nm "bytes"
                          -- see Note [Bytes label] in GHC.Cmm.CLabel
      m <- lookupHsSymbol interp pkgs_loaded addr_nm "bytes"
      case m of
        Just ptr -> return ptr
        Nothing -> linkFail "GHC.ByteCode.Linker.lookupAddr"
                     (unpackFS sym_to_find)

lookupPrimOp :: Interp -> PkgsLoaded -> PrimOp -> IO (RemotePtr ())
lookupPrimOp interp pkgs_loaded primop = do
  let sym_to_find = primopToCLabel primop "closure"
  m <- lookupHsSymbol interp pkgs_loaded (Id.idName $ primOpId primop) "closure"
  case m of
    Just p -> return (toRemotePtr p)
    Nothing -> linkFail "GHC.ByteCode.Linker.lookupCE(primop)" sym_to_find

resolvePtr
  :: Interp
  -> PkgsLoaded
  -> LinkerEnv
  -> NameEnv Int
  -> RemoteRef BreakArray
  -> BCOPtr
  -> IO ResolvedBCOPtr
resolvePtr interp pkgs_loaded le bco_ix breakarray ptr = case ptr of
  BCOPtrName nm
    | Just ix <- lookupNameEnv bco_ix nm
    -> return (ResolvedBCORef ix) -- ref to another BCO in this group

    | Just (_, rhv) <- lookupNameEnv (closure_env le) nm
    -> return (ResolvedBCOPtr (unsafeForeignRefToRemoteRef rhv))

    | otherwise
    -> assertPpr (isExternalName nm) (ppr nm) $
       do
          let sym_to_find = nameToCLabel nm "closure"
          m <- lookupHsSymbol interp pkgs_loaded nm "closure"
          case m of
            Just p -> return (ResolvedBCOStaticPtr (toRemotePtr p))
            Nothing -> linkFail "GHC.ByteCode.Linker.lookupCE" (unpackFS sym_to_find)

  BCOPtrPrimOp op
    -> ResolvedBCOStaticPtr <$> lookupPrimOp interp pkgs_loaded op

  BCOPtrBCO bco
    -> ResolvedBCOPtrBCO <$> linkBCO interp pkgs_loaded le bco_ix breakarray bco

  BCOPtrBreakArray
    -> return (ResolvedBCOPtrBreakArray breakarray)

-- | Look up the address of a Haskell symbol in the currently
-- loaded units.
--
-- See Note [Looking up symbols in the relevant objects].
lookupHsSymbol :: Interp -> PkgsLoaded -> Name -> String -> IO (Maybe (Ptr ()))
lookupHsSymbol interp pkgs_loaded nm sym_suffix = do
  massertPpr (isExternalName nm) (ppr nm)
  let sym_to_find = nameToCLabel nm sym_suffix
      pkg_id = moduleUnitId $ nameModule nm
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

linkFail :: String -> String -> IO a
linkFail who what
   = throwGhcExceptionIO (ProgramError $
        unlines [ "",who
                , "During interactive linking, GHCi couldn't find the following symbol:"
                , ' ' : ' ' : what
                , "This may be due to you not asking GHCi to load extra object files,"
                , "archives or DLLs needed by your current session.  Restart GHCi, specifying"
                , "the missing library using the -L/path/to/object/dir and -lmissinglibname"
                , "flags, or simply by naming the relevant files on the GHCi command line."
                , "Alternatively, this link failure might indicate a bug in GHCi."
                , "If you suspect the latter, please report this as a GHC bug:"
                , "  https://www.haskell.org/ghc/reportabug"
                ])


nameToCLabel :: Name -> String -> FastString
nameToCLabel n suffix = mkFastString label
  where
    encodeZ = zString . zEncodeFS
    (Module pkgKey modName) = assert (isExternalName n) $ case nameModule n of
        -- Primops are exported from GHC.Prim, their HValues live in GHC.PrimopWrappers
        -- See Note [Primop wrappers] in GHC.Builtin.PrimOps.
        mod | mod == gHC_PRIM -> gHC_PRIMOPWRAPPERS
        mod -> mod
    packagePart = encodeZ (unitFS pkgKey)
    modulePart  = encodeZ (moduleNameFS modName)
    occPart     = encodeZ $ occNameMangledFS (nameOccName n)

    label = concat
        [ if pkgKey == mainUnit then "" else packagePart ++ "_"
        , modulePart
        , '_':occPart
        , '_':suffix
        ]


-- See Note [Primop wrappers] in GHC.Builtin.PrimOps
primopToCLabel :: PrimOp -> String -> String
primopToCLabel primop suffix = concat
    [ "ghczmprim_GHCziPrimopWrappers_"
    , zString (zEncodeFS (occNameFS (primOpOcc primop)))
    , '_':suffix
    ]
