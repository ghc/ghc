%
% (c) The University of Glasgow 2000-2006
%
ByteCodeLink: Bytecode assembler and linker

\begin{code}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -optc-DNON_POSIX_SOURCE #-}

module ByteCodeLink (
        ClosureEnv, emptyClosureEnv, extendClosureEnv,
        linkBCO, lookupStaticPtr, lookupName
       ,lookupIE
  ) where

#include "HsVersions.h"

import ByteCodeItbls
import ByteCodeAsm
import ObjLink

import DynFlags
import BasicTypes
import Name
import NameEnv
import PrimOp
import Module
import FastString
import Panic
import Outputable
import Util

-- Standard libraries

import Data.Array.Base

import Control.Monad
import Control.Monad.ST ( stToIO )

import GHC.Arr          ( Array(..), STArray(..) )
import GHC.IO           ( IO(..) )
import GHC.Exts
import GHC.Ptr          ( castPtr )
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Linking interpretables into something we can run}
%*                                                                      *
%************************************************************************

\begin{code}
type ClosureEnv = NameEnv (Name, HValue)

emptyClosureEnv :: ClosureEnv
emptyClosureEnv = emptyNameEnv

extendClosureEnv :: ClosureEnv -> [(Name,HValue)] -> ClosureEnv
extendClosureEnv cl_env pairs
  = extendNameEnvList cl_env [ (n, (n,v)) | (n,v) <- pairs]
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Linking interpretables into something we can run}
%*                                                                      *
%************************************************************************

\begin{code}
{-
data BCO# = BCO# ByteArray#             -- instrs   :: Array Word16#
                 ByteArray#             -- literals :: Array Word32#
                 PtrArray#              -- ptrs     :: Array HValue
                 ByteArray#             -- itbls    :: Array Addr#
-}

linkBCO :: DynFlags -> ItblEnv -> ClosureEnv -> UnlinkedBCO -> IO HValue
linkBCO dflags ie ce ul_bco
   = do BCO bco# <- linkBCO' dflags ie ce ul_bco
        -- SDM: Why do we need mkApUpd0 here?  I *think* it's because
        -- otherwise top-level interpreted CAFs don't get updated
        -- after evaluation.   A top-level BCO will evaluate itself and
        -- return its value when entered, but it won't update itself.
        -- Wrapping the BCO in an AP_UPD thunk will take care of the
        -- update for us.
        --
        -- Update: the above is true, but now we also have extra invariants:
        --   (a) An AP thunk *must* point directly to a BCO
        --   (b) A zero-arity BCO *must* be wrapped in an AP thunk
        --   (c) An AP is always fully saturated, so we *can't* wrap
        --       non-zero arity BCOs in an AP thunk.
        --
        if (unlinkedBCOArity ul_bco > 0)
           then return (HValue (unsafeCoerce# bco#))
           else case mkApUpd0# bco# of { (# final_bco #) -> return (HValue final_bco) }


linkBCO' :: DynFlags -> ItblEnv -> ClosureEnv -> UnlinkedBCO -> IO BCO
linkBCO' dflags ie ce (UnlinkedBCO _ arity insns_barr bitmap literalsSS ptrsSS)
   -- Raises an IO exception on failure
   = do let literals = ssElts literalsSS
            ptrs     = ssElts ptrsSS

        linked_literals <- mapM (lookupLiteral dflags ie) literals

        let n_literals = sizeSS literalsSS
            n_ptrs     = sizeSS ptrsSS

        ptrs_arr <- mkPtrsArray dflags ie ce n_ptrs ptrs

        let
            !ptrs_parr = case ptrs_arr of Array _lo _hi _n parr -> parr

            litRange
             | n_literals > 0     = (0, fromIntegral n_literals - 1)
             | otherwise          = (1, 0)
            literals_arr :: UArray Word Word
            literals_arr = listArray litRange linked_literals
            !literals_barr = case literals_arr of UArray _lo _hi _n barr -> barr

            !(I# arity#)  = arity

        newBCO insns_barr literals_barr ptrs_parr arity# bitmap


-- we recursively link any sub-BCOs while making the ptrs array
mkPtrsArray :: DynFlags -> ItblEnv -> ClosureEnv -> Word -> [BCOPtr] -> IO (Array Word HValue)
mkPtrsArray dflags ie ce n_ptrs ptrs = do
  let ptrRange = if n_ptrs > 0 then (0, n_ptrs-1) else (1, 0)
  marr <- newArray_ ptrRange
  let
    fill (BCOPtrName n)     i = do
        ptr <- lookupName ce n
        unsafeWrite marr i ptr
    fill (BCOPtrPrimOp op)  i = do
        ptr <- lookupPrimOp op
        unsafeWrite marr i ptr
    fill (BCOPtrBCO ul_bco) i = do
        BCO bco# <- linkBCO' dflags ie ce ul_bco
        writeArrayBCO marr i bco#
    fill (BCOPtrBreakInfo brkInfo) i =
        unsafeWrite marr i (HValue (unsafeCoerce# brkInfo))
    fill (BCOPtrArray brkArray) i =
        unsafeWrite marr i (HValue (unsafeCoerce# brkArray))
  zipWithM_ fill ptrs [0..]
  unsafeFreeze marr

newtype IOArray i e = IOArray (STArray RealWorld i e)

instance MArray IOArray e IO where
    getBounds (IOArray marr) = stToIO $ getBounds marr
    getNumElements (IOArray marr) = stToIO $ getNumElements marr
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOArray marr)
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOArray marr)
    unsafeRead (IOArray marr) i = stToIO (unsafeRead marr i)
    unsafeWrite (IOArray marr) i e = stToIO (unsafeWrite marr i e)

-- XXX HACK: we should really have a new writeArray# primop that takes a BCO#.
writeArrayBCO :: IOArray Word a -> Int -> BCO# -> IO ()
writeArrayBCO (IOArray (STArray _ _ _ marr#)) (I# i#) bco# = IO $ \s# ->
  case (unsafeCoerce# writeArray#) marr# i# bco# s# of { s# ->
  (# s#, () #) }

{-
writeArrayMBA :: IOArray Int a -> Int -> MutableByteArray# a -> IO ()
writeArrayMBA (IOArray (STArray _ _ marr#)) (I# i#) mba# = IO $ \s# ->
  case (unsafeCoerce# writeArray#) marr# i# bco# s# of { s# ->
  (# s#, () #) }
-}

data BCO = BCO BCO#

newBCO :: ByteArray# -> ByteArray# -> Array# a -> Int# -> ByteArray# -> IO BCO
newBCO instrs lits ptrs arity bitmap
   = IO $ \s -> case newBCO# instrs lits ptrs arity bitmap s of
                  (# s1, bco #) -> (# s1, BCO bco #)


lookupLiteral :: DynFlags -> ItblEnv -> BCONPtr -> IO Word
lookupLiteral _      _  (BCONPtrWord lit) = return lit
lookupLiteral _      _  (BCONPtrLbl  sym) = do Ptr a# <- lookupStaticPtr sym
                                               return (W# (int2Word# (addr2Int# a#)))
lookupLiteral dflags ie (BCONPtrItbl nm)  = do Ptr a# <- lookupIE dflags ie nm
                                               return (W# (int2Word# (addr2Int# a#)))

lookupStaticPtr :: FastString -> IO (Ptr ())
lookupStaticPtr addr_of_label_string
   = do let label_to_find = unpackFS addr_of_label_string
        m <- lookupSymbol label_to_find
        case m of
           Just ptr -> return ptr
           Nothing  -> linkFail "ByteCodeLink: can't find label"
                                label_to_find

lookupPrimOp :: PrimOp -> IO HValue
lookupPrimOp primop
   = do let sym_to_find = primopToCLabel primop "closure"
        m <- lookupSymbol sym_to_find
        case m of
           Just (Ptr addr) -> case addrToAny# addr of
                                 (# a #) -> return (HValue a)
           Nothing -> linkFail "ByteCodeLink.lookupCE(primop)" sym_to_find

lookupName :: ClosureEnv -> Name -> IO HValue
lookupName ce nm
   = case lookupNameEnv ce nm of
        Just (_,aa) -> return aa
        Nothing
           -> ASSERT2(isExternalName nm, ppr nm)
              do let sym_to_find = nameToCLabel nm "closure"
                 m <- lookupSymbol sym_to_find
                 case m of
                    Just (Ptr addr) -> case addrToAny# addr of
                                          (# a #) -> return (HValue a)
                    Nothing         -> linkFail "ByteCodeLink.lookupCE" sym_to_find

lookupIE :: DynFlags -> ItblEnv -> Name -> IO (Ptr a)
lookupIE dflags ie con_nm
   = case lookupNameEnv ie con_nm of
        Just (_, a) -> return (castPtr (itblCode dflags a))
        Nothing
           -> do -- try looking up in the object files.
                 let sym_to_find1 = nameToCLabel con_nm "con_info"
                 m <- lookupSymbol sym_to_find1
                 case m of
                    Just addr -> return addr
                    Nothing
                       -> do -- perhaps a nullary constructor?
                             let sym_to_find2 = nameToCLabel con_nm "static_info"
                             n <- lookupSymbol sym_to_find2
                             case n of
                                Just addr -> return addr
                                Nothing   -> linkFail "ByteCodeLink.lookupIE"
                                                (sym_to_find1 ++ " or " ++ sym_to_find2)

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

-- HACKS!!!  ToDo: cleaner
nameToCLabel :: Name -> String{-suffix-} -> String
nameToCLabel n suffix
   = if pkgid /= mainPackageId
        then package_part ++ '_': qual_name
        else qual_name
  where
        pkgid = modulePackageId mod
        mod = ASSERT( isExternalName n ) nameModule n
        package_part = zString (zEncodeFS (packageIdFS (modulePackageId mod)))
        module_part  = zString (zEncodeFS (moduleNameFS (moduleName mod)))
        occ_part     = zString (zEncodeFS (occNameFS (nameOccName n)))
        qual_name = module_part ++ '_':occ_part ++ '_':suffix


primopToCLabel :: PrimOp -> String{-suffix-} -> String
primopToCLabel primop suffix
   = let str = "ghczmprim_GHCziPrimopWrappers_" ++ zString (zEncodeFS (occNameFS (primOpOcc primop))) ++ '_':suffix
     in --trace ("primopToCLabel: " ++ str)
        str
\end{code}

