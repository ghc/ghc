%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[module_ST]{The State Transformer Monad, @ST@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module ST 
      (
	ST                  -- abstract, instance of Functor, Monad.
      , runST		    -- :: (forall s. ST s a) -> a
      , fixST		    -- :: (a -> ST s a) -> ST s a
      , unsafeInterleaveST  -- :: ST s a -> ST s a

      , STRef
      , newSTRef
      , readSTRef
      , writeSTRef
      
      , unsafeIOToST
      , stToIO
      
      , STArray
      , newSTArray
      , readSTArray
      , writeSTArray
      , boundsSTArray
      , thawSTArray
      , freezeSTArray
      , unsafeFreezeSTArray
#ifndef __HUGS__
-- no 'good' reason, just doesn't support it right now.
      , unsafeThawSTArray
#endif

      ) where

#ifdef __HUGS__
import PreludeBuiltin
#define MutableVar Ref
#define readVar    primReadRef
#define writeVar   primWriteRef
#define newVar     primNewRef
#else
import PrelArr
import PrelST
import PrelBase	( Eq(..), Int, Bool, ($), ()(..), unsafeCoerce# )
import PrelIOBase ( IO(..), stToIO )
#endif
import Monad
import Ix

\end{code}

%*********************************************************
%*							*
\subsection{Variables}
%*							*
%*********************************************************

\begin{code}
newtype STRef s a = STRef (MutableVar s a) 
    deriving Eq

newSTRef :: a -> ST s (STRef s a)
newSTRef v = newVar v >>= \ var -> return (STRef var)

readSTRef :: STRef s a -> ST s a
readSTRef (STRef var) = readVar var

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef (STRef var) v = writeVar var v
\end{code}

%*********************************************************
%*							*
\subsection{Arrays}
%*							*
%*********************************************************

\begin{code}
newSTArray 		:: Ix ix => (ix,ix) -> elt -> ST s (STArray s ix elt)
writeSTArray	  	:: Ix ix => STArray s ix elt -> ix -> elt -> ST s () 
readSTArray   		:: Ix ix => STArray s ix elt -> ix -> ST s elt 
boundsSTArray     	:: Ix ix => STArray s ix elt -> (ix, ix)  
thawSTArray 		:: Ix ix => Array ix elt -> ST s (STArray s ix elt)
freezeSTArray	  	:: Ix ix => STArray s ix elt -> ST s (Array ix elt)
unsafeFreezeSTArray 	:: Ix ix => STArray s ix elt -> ST s (Array ix elt)

#ifndef __HUGS__
-- see export list comment..
unsafeThawSTArray 	:: Ix ix => Array ix elt -> ST s (STArray s ix elt)
#endif

#ifdef __HUGS__
data STArray s ix elt = STArray (ix,ix) (PrimMutableArray s elt)
  deriving Eq

newSTArray ixs elt = do
  { arr <- primNewArray (rangeSize ixs) elt
  ; return (STArray ixs arr)
  }

boundsSTArray (STArray ixs arr)        = ixs
readSTArray   (STArray ixs arr) ix     = primReadArray arr (index ixs ix)
writeSTArray  (STArray ixs arr) ix elt = primWriteArray arr (index ixs ix) elt
freezeSTArray (STArray ixs arr)        = do
  { arr' <- primFreezeArray arr
  ; return (Array ixs arr')
  }

unsafeFreezeSTArray (STArray ixs arr)  = do 
  { arr' <- primUnsafeFreezeArray arr
  ; return (Array ixs arr')
  }

thawSTArray (Array ixs arr) = do
  { arr' <- primThawArray arr
  ; return (STArray ixs arr')
  }

primFreezeArray :: PrimMutableArray s a -> ST s (PrimArray a)
primFreezeArray arr = do
  { let n = primSizeMutableArray arr
  ; arr' <- primNewArray n arrEleBottom
  ; mapM_ (copy arr arr') [0..n-1]
  ; primUnsafeFreezeArray arr'
  }
 where
  copy arr arr' i = do { x <- primReadArray arr i; primWriteArray arr' i x }
  arrEleBottom = error "primFreezeArray: panic"

primThawArray :: PrimArray a -> ST s (PrimMutableArray s a)
primThawArray arr = do
  { let n = primSizeArray arr
  ; arr' <- primNewArray n arrEleBottom
  ; mapM_ (copy arr arr') [0..n-1]
  ; return arr'
  }
 where
  copy arr arr' i = primWriteArray arr' i (primIndexArray arr i)
  arrEleBottom = error "primFreezeArray: panic"
#else
newtype STArray s ix elt = STArray (MutableArray s ix elt)
    deriving Eq

newSTArray ixs elt = 
    newArray ixs elt >>= \arr -> 
    return (STArray arr)

boundsSTArray (STArray arr) = boundsOfArray arr

readSTArray (STArray arr) ix = readArray arr ix

writeSTArray (STArray arr) ix elt = writeArray arr ix elt

thawSTArray arr = thawArray arr >>= \starr -> return (STArray starr)

freezeSTArray (STArray arr) = freezeArray arr

unsafeFreezeSTArray (STArray arr) = unsafeFreezeArray arr
unsafeThawSTArray arr = unsafeThawArray arr >>= \ marr -> return (STArray marr)

#endif
\end{code}


\begin{code}
unsafeIOToST	   :: IO a -> ST s a
#ifdef __HUGS__
unsafeIOToST = primUnsafeCoerce
#else
unsafeIOToST (IO io) = ST $ \ s ->
    case ((unsafeCoerce# io) s) of
      (#  new_s, a #) -> unsafeCoerce# (STret new_s a)
--      IOfail new_s e -> error ("I/O Error (unsafeIOToST): " ++ showsPrec 0 e "\n")
#endif
\end{code}
