{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -optc-DNON_POSIX_SOURCE #-}
--
--  (c) The University of Glasgow 2002-2006
--

-- | ByteCodeLink: Bytecode assembler and linker
module ByteCodeLink (
        ClosureEnv, emptyClosureEnv, extendClosureEnv,
        linkBCO, lookupStaticPtr,
        lookupIE,
        nameToCLabel, linkFail
  ) where

#include "HsVersions.h"

import GhcPrelude

import GHCi.RemoteTypes
import GHCi.ResolvedBCO
import GHCi.BreakArray
import SizedSeq

import GHCi
import ByteCodeTypes
import HscTypes
import Name
import NameEnv
import PrimOp
import Module
import FastString
import Panic
import Outputable
import Util

-- Standard libraries
import Data.Array.Unboxed
import Foreign.Ptr
import GHC.Exts

{-
  Linking interpretables into something we can run
-}

type ClosureEnv = NameEnv (Name, ForeignHValue)

emptyClosureEnv :: ClosureEnv
emptyClosureEnv = emptyNameEnv

extendClosureEnv :: ClosureEnv -> [(Name,ForeignHValue)] -> ClosureEnv
extendClosureEnv cl_env pairs
  = extendNameEnvList cl_env [ (n, (n,v)) | (n,v) <- pairs]

{-
  Linking interpretables into something we can run
-}

linkBCO
  :: HscEnv -> ItblEnv -> ClosureEnv -> NameEnv Int -> RemoteRef BreakArray
  -> UnlinkedBCO
  -> IO ResolvedBCO
linkBCO hsc_env ie ce bco_ix breakarray
           (UnlinkedBCO _ arity insns bitmap lits0 ptrs0) = do
  -- fromIntegral Word -> Word64 should be a no op if Word is Word64
  -- otherwise it will result in a cast to longlong on 32bit systems.
  lits <- mapM (fmap fromIntegral . lookupLiteral hsc_env ie) (ssElts lits0)
  ptrs <- mapM (resolvePtr hsc_env ie ce bco_ix breakarray) (ssElts ptrs0)
  return (ResolvedBCO isLittleEndian arity insns bitmap
              (listArray (0, fromIntegral (sizeSS lits0)-1) lits)
              (addListToSS emptySS ptrs))

lookupLiteral :: HscEnv -> ItblEnv -> BCONPtr -> IO Word
lookupLiteral _ _ (BCONPtrWord lit) = return lit
lookupLiteral hsc_env _ (BCONPtrLbl  sym) = do
  Ptr a# <- lookupStaticPtr hsc_env sym
  return (W# (int2Word# (addr2Int# a#)))
lookupLiteral hsc_env ie (BCONPtrItbl nm)  = do
  Ptr a# <- lookupIE hsc_env ie nm
  return (W# (int2Word# (addr2Int# a#)))
lookupLiteral _ _ (BCONPtrStr _) =
  -- should be eliminated during assembleBCOs
  panic "lookupLiteral: BCONPtrStr"

lookupStaticPtr :: HscEnv -> FastString -> IO (Ptr ())
lookupStaticPtr hsc_env addr_of_label_string = do
  m <- lookupSymbol hsc_env addr_of_label_string
  case m of
    Just ptr -> return ptr
    Nothing  -> linkFail "ByteCodeLink: can't find label"
                  (unpackFS addr_of_label_string)

lookupIE :: HscEnv -> ItblEnv -> Name -> IO (Ptr ())
lookupIE hsc_env ie con_nm =
  case lookupNameEnv ie con_nm of
    Just (_, ItblPtr a) -> return (fromRemotePtr (castRemotePtr a))
    Nothing -> do -- try looking up in the object files.
       let sym_to_find1 = nameToCLabel con_nm "con_info"
       m <- lookupSymbol hsc_env sym_to_find1
       case m of
          Just addr -> return addr
          Nothing
             -> do -- perhaps a nullary constructor?
                   let sym_to_find2 = nameToCLabel con_nm "static_info"
                   n <- lookupSymbol hsc_env sym_to_find2
                   case n of
                      Just addr -> return addr
                      Nothing   -> linkFail "ByteCodeLink.lookupIE"
                                      (unpackFS sym_to_find1 ++ " or " ++
                                       unpackFS sym_to_find2)

lookupPrimOp :: HscEnv -> PrimOp -> IO (RemotePtr ())
lookupPrimOp hsc_env primop = do
  let sym_to_find = primopToCLabel primop "closure"
  m <- lookupSymbol hsc_env (mkFastString sym_to_find)
  case m of
    Just p -> return (toRemotePtr p)
    Nothing -> linkFail "ByteCodeLink.lookupCE(primop)" sym_to_find

resolvePtr
  :: HscEnv -> ItblEnv -> ClosureEnv -> NameEnv Int -> RemoteRef BreakArray
  -> BCOPtr
  -> IO ResolvedBCOPtr
resolvePtr hsc_env _ie ce bco_ix _ (BCOPtrName nm)
  | Just ix <- lookupNameEnv bco_ix nm =
    return (ResolvedBCORef ix) -- ref to another BCO in this group
  | Just (_, rhv) <- lookupNameEnv ce nm =
    return (ResolvedBCOPtr (unsafeForeignRefToRemoteRef rhv))
  | otherwise =
    ASSERT2(isExternalName nm, ppr nm)
    do let sym_to_find = nameToCLabel nm "closure"
       m <- lookupSymbol hsc_env sym_to_find
       case m of
         Just p -> return (ResolvedBCOStaticPtr (toRemotePtr p))
         Nothing -> linkFail "ByteCodeLink.lookupCE" (unpackFS sym_to_find)
resolvePtr hsc_env _ _ _ _ (BCOPtrPrimOp op) =
  ResolvedBCOStaticPtr <$> lookupPrimOp hsc_env op
resolvePtr hsc_env ie ce bco_ix breakarray (BCOPtrBCO bco) =
  ResolvedBCOPtrBCO <$> linkBCO hsc_env ie ce bco_ix breakarray bco
resolvePtr _ _ _ _ breakarray BCOPtrBreakArray =
  return (ResolvedBCOPtrBreakArray breakarray)

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
                , "If you suspect the latter, please send a bug report to:"
                , "  glasgow-haskell-bugs@haskell.org"
                ])


nameToCLabel :: Name -> String -> FastString
nameToCLabel n suffix = mkFastString label
  where
    encodeZ = zString . zEncodeFS
    (Module pkgKey modName) = ASSERT( isExternalName n ) nameModule n
    packagePart = encodeZ (unitIdFS pkgKey)
    modulePart  = encodeZ (moduleNameFS modName)
    occPart     = encodeZ (occNameFS (nameOccName n))

    label = concat
        [ if pkgKey == mainUnitId then "" else packagePart ++ "_"
        , modulePart
        , '_':occPart
        , '_':suffix
        ]


primopToCLabel :: PrimOp -> String -> String
primopToCLabel primop suffix = concat
    [ "ghczmprim_GHCziPrimopWrappers_"
    , zString (zEncodeFS (occNameFS (primOpOcc primop)))
    , '_':suffix
    ]
