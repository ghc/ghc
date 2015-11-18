{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnboxedTuples #-}
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

import GHCi.RemoteTypes
import GHCi.ResolvedBCO
import GHCi.InfoTable
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
import GHC.IO           ( IO(..) )
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
  :: HscEnv -> ItblEnv -> ClosureEnv -> NameEnv Int -> UnlinkedBCO
  -> IO ResolvedBCO
linkBCO hsc_env ie ce bco_ix
           (UnlinkedBCO _ arity insns bitmap lits0 ptrs0) = do
  lits <- mapM (lookupLiteral hsc_env ie) (ssElts lits0)
  ptrs <- mapM (resolvePtr hsc_env ie ce bco_ix) (ssElts ptrs0)
  return (ResolvedBCO arity insns bitmap
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
lookupLiteral hsc_env _ (BCONPtrStr bs) = do
  fromIntegral . ptrToWordPtr <$> mallocData hsc_env bs

lookupStaticPtr :: HscEnv -> FastString -> IO (Ptr ())
lookupStaticPtr hsc_env addr_of_label_string = do
  m <- lookupSymbol hsc_env addr_of_label_string
  case m of
    Just ptr -> return ptr
    Nothing  -> linkFail "ByteCodeLink: can't find label"
                  (unpackFS addr_of_label_string)

lookupIE :: HscEnv -> ItblEnv -> Name -> IO (Ptr a)
lookupIE hsc_env ie con_nm =
  case lookupNameEnv ie con_nm of
    Just (_, ItblPtr a) -> return (castPtr (conInfoPtr a))
    Nothing -> do -- try looking up in the object files.
       let sym_to_find1 = nameToCLabel con_nm "con_info"
       m <- lookupSymbol hsc_env sym_to_find1
       case m of
          Just addr -> return (castPtr addr)
          Nothing
             -> do -- perhaps a nullary constructor?
                   let sym_to_find2 = nameToCLabel con_nm "static_info"
                   n <- lookupSymbol hsc_env sym_to_find2
                   case n of
                      Just addr -> return (castPtr addr)
                      Nothing   -> linkFail "ByteCodeLink.lookupIE"
                                      (unpackFS sym_to_find1 ++ " or " ++
                                       unpackFS sym_to_find2)

lookupPrimOp :: HscEnv -> PrimOp -> IO RemotePtr
lookupPrimOp hsc_env primop = do
  let sym_to_find = primopToCLabel primop "closure"
  m <- lookupSymbol hsc_env (mkFastString sym_to_find)
  case m of
    Just p -> return (toRemotePtr p)
    Nothing -> linkFail "ByteCodeLink.lookupCE(primop)" sym_to_find

resolvePtr
  :: HscEnv -> ItblEnv -> ClosureEnv -> NameEnv Int -> BCOPtr
  -> IO ResolvedBCOPtr
resolvePtr hsc_env _ie ce bco_ix (BCOPtrName nm)
  | Just ix <- lookupNameEnv bco_ix nm =
    return (ResolvedBCORef ix) -- ref to another BCO in this group
  | Just (_, rhv) <- lookupNameEnv ce nm =
    return (ResolvedBCOPtr (unsafeForeignHValueToHValueRef rhv))
  | otherwise =
    ASSERT2(isExternalName nm, ppr nm)
    do let sym_to_find = nameToCLabel nm "closure"
       m <- lookupSymbol hsc_env sym_to_find
       case m of
         Just p -> return (ResolvedBCOStaticPtr (toRemotePtr p))
         Nothing -> linkFail "ByteCodeLink.lookupCE" (unpackFS sym_to_find)
resolvePtr hsc_env _ _ _ (BCOPtrPrimOp op) =
  ResolvedBCOStaticPtr <$> lookupPrimOp hsc_env op
resolvePtr hsc_env ie ce bco_ix (BCOPtrBCO bco) =
  ResolvedBCOPtrBCO <$> linkBCO hsc_env ie ce bco_ix bco
resolvePtr _ _ _ _ (BCOPtrBreakInfo break_info) =
  return (ResolvedBCOPtrLocal (unsafeCoerce# break_info))
resolvePtr _ _ _ _ (BCOPtrArray break_array) =
  return (ResolvedBCOPtrLocal (unsafeCoerce# break_array))

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
