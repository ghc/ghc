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
import ByteCodeAsm	( UnlinkedBCO(..), sizeSS, ssElts )
import ObjLink		( lookupSymbol )

import Name		( Name,  nameModule, nameOccName, isExternalName )
import NameEnv
import OccName		( occNameString )
import PrimOp		( PrimOp, primOpOcc )
import Module		( moduleString  )
import FastString	( FastString(..), unpackFS )
import Outputable
import Panic            ( GhcException(..) )

-- Standard libraries
import GHC.Word		( Word(..) )

import Data.Array.IArray ( array )
import Data.Array.Base	( UArray(..) )
import Foreign		( Word16 )

import Control.Exception ( throwDyn )

import GHC.Exts		( BCO#, newBCO#, unsafeCoerce#, 
			  ByteArray#, Array#, addrToHValue#, mkApUpd0# )

import GHC.Arr		( Array(..) )
import GHC.IOBase	( IO(..) )
import GHC.Ptr		( Ptr(..) )
\end{code}


%************************************************************************
%*									*
\subsection{Linking interpretables into something we can run}
%*									*
%************************************************************************

\begin{code}
type ClosureEnv = NameEnv (Name, HValue)
data HValue     = HValue  -- dummy type, actually a pointer to some Real Code.

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
linkBCO ie ce (UnlinkedBCO nm insnsSS literalsSS ptrsSS itblsSS)
-- Raises an IO exception on failure
   = do let insns    = ssElts insnsSS
 	    literals = ssElts literalsSS
	    ptrs     = ssElts ptrsSS
	    itbls    = ssElts itblsSS

        linked_ptrs     <- mapM (lookupCE ce) ptrs
        linked_itbls    <- mapM (lookupIE ie) itbls
        linked_literals <- mapM lookupLiteral literals

        let n_insns    = sizeSS insnsSS
            n_literals = sizeSS literalsSS
            n_ptrs     = sizeSS ptrsSS
            n_itbls    = sizeSS itblsSS

        let ptrs_arr = array (0, n_ptrs-1) (indexify linked_ptrs)
                       :: Array Int HValue
            ptrs_parr = case ptrs_arr of Array lo hi parr -> parr

            itbls_arr = array (0, n_itbls-1) (indexify linked_itbls)
                        :: UArray Int ItblPtr
            itbls_barr = case itbls_arr of UArray lo hi barr -> barr

            insns_arr | n_insns > 65535
                      = panic "linkBCO: >= 64k insns in BCO"
                      | otherwise 
                      = array (0, n_insns) 
                              (indexify (fromIntegral n_insns:insns))
                        :: UArray Int Word16
            insns_barr = case insns_arr of UArray lo hi barr -> barr

            literals_arr = array (0, n_literals-1) (indexify linked_literals)
                           :: UArray Int Word
            literals_barr = case literals_arr of UArray lo hi barr -> barr

            indexify :: [a] -> [(Int, a)]
            indexify xs = zip [0..] xs

        BCO bco# <- newBCO insns_barr literals_barr ptrs_parr itbls_barr

        -- WAS: return (unsafeCoerce# bco#)
        case mkApUpd0# (unsafeCoerce# bco#) of
           (# final_bco #) -> return final_bco


data BCO = BCO BCO#

newBCO :: ByteArray# -> ByteArray# -> Array# a -> ByteArray# -> IO BCO
newBCO a b c d
   = IO (\s -> case newBCO# a b c d s of (# s1, bco #) -> (# s1, BCO bco #))


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

lookupCE :: ClosureEnv -> Either Name PrimOp -> IO HValue
lookupCE ce (Right primop)
   = do let sym_to_find = primopToCLabel primop "closure"
        m <- lookupSymbol sym_to_find
        case m of
           Just (Ptr addr) -> case addrToHValue# addr of
                                 (# hval #) -> return hval
           Nothing -> linkFail "ByteCodeLink.lookupCE(primop)" sym_to_find

lookupCE ce (Left nm)
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
   = moduleString (nameModule n)
     ++ '_':occNameString (nameOccName n) ++ '_':suffix

primopToCLabel :: PrimOp -> String{-suffix-} -> String
primopToCLabel primop suffix
   = let str = "GHCziPrimopWrappers_" ++ occNameString (primOpOcc primop) ++ '_':suffix
     in --trace ("primopToCLabel: " ++ str)
        str
\end{code}

