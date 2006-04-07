%
% (c) The University of Glasgow 2000
%
\section[ByteCodeLink]{Bytecode assembler and linker}

\begin{code}

{-# OPTIONS -optc-DNON_POSIX_SOURCE #-}

module ByteCodeLink ( 
	HValue, 
	ClosureEnv, emptyClosureEnv, extendClosureEnv,
	linkBCO, lookupStaticPtr
  ) where

#include "HsVersions.h"

import ByteCodeItbls	( ItblEnv, ItblPtr )
import ByteCodeAsm	( UnlinkedBCO(..), BCOPtr(..), sizeSS, ssElts )
import ObjLink		( lookupSymbol )

import Name		( Name,  nameModule, nameOccName, isExternalName )
import NameEnv
import OccName		( occNameFS )
import PrimOp		( PrimOp, primOpOcc )
import Module		( moduleFS )
import FastString	( FastString(..), unpackFS, zEncodeFS )
import Outputable
import Panic            ( GhcException(..) )

-- Standard libraries
import GHC.Word		( Word(..) )

import Data.Array.IArray ( listArray )
import Data.Array.Base
import GHC.Arr		( STArray(..) )

import Control.Exception ( throwDyn )
import Control.Monad	( zipWithM )
import Control.Monad.ST ( stToIO )

import GHC.Exts		( BCO#, newBCO#, unsafeCoerce#, Int#,
			  ByteArray#, Array#, addrToHValue#, mkApUpd0# )

import GHC.Arr		( Array(..) )
import GHC.IOBase	( IO(..) )
import GHC.Ptr		( Ptr(..) )
import GHC.Base		( writeArray#, RealWorld, Int(..) )
\end{code}


%************************************************************************
%*									*
\subsection{Linking interpretables into something we can run}
%*									*
%************************************************************************

\begin{code}
type ClosureEnv = NameEnv (Name, HValue)
newtype HValue = HValue (forall a . a)

emptyClosureEnv = emptyNameEnv

extendClosureEnv :: ClosureEnv -> [(Name,HValue)] -> ClosureEnv
extendClosureEnv cl_env pairs
  = extendNameEnvList cl_env [ (n, (n,v)) | (n,v) <- pairs]
\end{code}


%************************************************************************
%*									*
\subsection{Linking interpretables into something we can run}
%*									*
%************************************************************************

\begin{code}
{- 
data BCO# = BCO# ByteArray# 		-- instrs   :: Array Word16#
                 ByteArray# 		-- literals :: Array Word32#
                 PtrArray# 		-- ptrs     :: Array HValue
                 ByteArray#		-- itbls    :: Array Addr#
-}

linkBCO :: ItblEnv -> ClosureEnv -> UnlinkedBCO -> IO HValue
linkBCO ie ce ul_bco
   = do BCO bco# <- linkBCO' ie ce ul_bco
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
	   then return (unsafeCoerce# bco#)
	   else case mkApUpd0# bco# of { (# final_bco #) -> return final_bco }


linkBCO' :: ItblEnv -> ClosureEnv -> UnlinkedBCO -> IO BCO
linkBCO' ie ce (UnlinkedBCO nm arity insns_barr bitmap literalsSS ptrsSS itblsSS)
   -- Raises an IO exception on failure
   = do let literals = ssElts literalsSS
	    ptrs     = ssElts ptrsSS
	    itbls    = ssElts itblsSS

        linked_itbls    <- mapM (lookupIE ie) itbls
        linked_literals <- mapM lookupLiteral literals

        let n_literals = sizeSS literalsSS
            n_ptrs     = sizeSS ptrsSS
            n_itbls    = sizeSS itblsSS

	ptrs_arr <- mkPtrsArray ie ce n_ptrs ptrs

        let 
            ptrs_parr = case ptrs_arr of Array lo hi parr -> parr

            itbls_arr = listArray (0, n_itbls-1) linked_itbls
                        :: UArray Int ItblPtr
            itbls_barr = case itbls_arr of UArray lo hi barr -> barr

            literals_arr = listArray (0, n_literals-1) linked_literals
                           :: UArray Int Word
            literals_barr = case literals_arr of UArray lo hi barr -> barr

	    (I# arity#)  = arity

        newBCO insns_barr literals_barr ptrs_parr itbls_barr arity# bitmap


-- we recursively link any sub-BCOs while making the ptrs array
mkPtrsArray :: ItblEnv -> ClosureEnv -> Int -> [BCOPtr] -> IO (Array Int HValue)
mkPtrsArray ie ce n_ptrs ptrs = do
  marr <- newArray_ (0, n_ptrs-1)
  let 
    fill (BCOPtrName n)     i = do
	ptr <- lookupName ce n
	unsafeWrite marr i ptr
    fill (BCOPtrPrimOp op)  i = do
 	ptr <- lookupPrimOp op
	unsafeWrite marr i ptr
    fill (BCOPtrBCO ul_bco) i = do
	BCO bco# <- linkBCO' ie ce ul_bco
	writeArrayBCO marr i bco#
  zipWithM fill ptrs [0..]
  unsafeFreeze marr

newtype IOArray i e = IOArray (STArray RealWorld i e)

instance HasBounds IOArray where
    bounds (IOArray marr) = bounds marr

instance MArray IOArray e IO where
    newArray lu init = stToIO $ do
        marr <- newArray lu init; return (IOArray marr)
    newArray_ lu = stToIO $ do
        marr <- newArray_ lu; return (IOArray marr)
    unsafeRead (IOArray marr) i = stToIO (unsafeRead marr i)
    unsafeWrite (IOArray marr) i e = stToIO (unsafeWrite marr i e)

-- XXX HACK: we should really have a new writeArray# primop that takes a BCO#.
writeArrayBCO :: IOArray Int a -> Int -> BCO# -> IO ()
writeArrayBCO (IOArray (STArray _ _ marr#)) (I# i#) bco# = IO $ \s# ->
  case (unsafeCoerce# writeArray#) marr# i# bco# s# of { s# ->
  (# s#, () #) }

data BCO = BCO BCO#

newBCO :: ByteArray# -> ByteArray# -> Array# a
	 -> ByteArray# -> Int# -> ByteArray# -> IO BCO
newBCO instrs lits ptrs itbls arity bitmap
   = IO $ \s -> case newBCO# instrs lits ptrs itbls arity bitmap s of 
		  (# s1, bco #) -> (# s1, BCO bco #)


lookupLiteral :: Either Word FastString -> IO Word
lookupLiteral (Left lit)  = return lit
lookupLiteral (Right sym) = do Ptr addr <- lookupStaticPtr sym
			       return (W# (unsafeCoerce# addr)) 
     -- Can't be bothered to find the official way to convert Addr# to Word#;
     -- the FFI/Foreign designers make it too damn difficult
     -- Hence we apply the Blunt Instrument, which works correctly
     -- on all reasonable architectures anyway

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
           Just (Ptr addr) -> case addrToHValue# addr of
                                 (# hval #) -> return hval
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
                    Just (Ptr addr) -> case addrToHValue# addr of
                                          (# hval #) -> return hval
                    Nothing         -> linkFail "ByteCodeLink.lookupCE" sym_to_find

lookupIE :: ItblEnv -> Name -> IO (Ptr a)
lookupIE ie con_nm 
   = case lookupNameEnv ie con_nm of
        Just (_, Ptr a) -> return (Ptr a)
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
   = throwDyn (ProgramError $
        unlines [ ""
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
   = unpackFS (zEncodeFS (moduleFS (nameModule n)))
     ++ '_': unpackFS (zEncodeFS (occNameFS (nameOccName n))) ++ '_':suffix

primopToCLabel :: PrimOp -> String{-suffix-} -> String
primopToCLabel primop suffix
   = let str = "GHCziPrimopWrappers_" ++ unpackFS (zEncodeFS (occNameFS (primOpOcc primop))) ++ '_':suffix
     in --trace ("primopToCLabel: " ++ str)
        str
\end{code}

