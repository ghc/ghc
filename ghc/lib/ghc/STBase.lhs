%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[STBase]{The @ST@ and @PrimIO@ monads}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module STBase where

import Monad
import PrelBase
import GHC
\end{code}

%*********************************************************
%*							*
\subsection{The @ST@ monad}
%*							*
%*********************************************************

The state-transformer monad proper.  By default the monad is strict;
too many people got bitten by space leaks when it was lazy.

\begin{code}
newtype ST s a = ST (State# s -> STret s a)

data STret s a = STret (State# s) a

runST (ST m)
  = case m realWorld# of
      STret _ r -> r

instance Monad (ST s) where
    {-# INLINE return #-}
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    return x = ST $ \ s -> STret s x
    m >> k   =  m >>= \ _ -> k

    (ST m) >>= k
      = ST $ \ s ->
	case (m s) of { STret new_s r ->
	case (k r) of { ST k2 ->
	(k2 new_s) }}



-- here for backward compatibility:

{-# INLINE returnST #-}
returnST :: a -> ST s a
thenST	 :: ST s a -> (a -> ST s b) -> ST s b
seqST	 :: ST s a -> ST s b -> ST s b

returnST = return
thenST   = (>>=)
seqST	 = (>>)

fixST :: (a -> ST s a) -> ST s a
fixST k = ST $ \ s ->
    let (ST k_r)  = k r
	ans       = k_r s
	STret _ r = ans
    in
    ans

-- more backward compatibility stuff:
listST		:: [ST s a] -> ST s [a]
mapST		:: (a -> ST s b) -> [a] -> ST s [b]
mapAndUnzipST	:: (a -> ST s (b,c)) -> [a] -> ST s ([b],[c])

listST		= accumulate
mapST		= mapM
mapAndUnzipST	= mapAndUnzipM

\end{code}


%*********************************************************
%*							*
\subsection{The @PrimIO@ monad}
%*							*
%*********************************************************

\begin{code}
type PrimIO a = ST RealWorld a

fixPrimIO :: (a -> PrimIO a) -> PrimIO a
fixPrimIO = fixST

-- the following functions are now there for backward compatibility mostly:

{-# GENERATE_SPECS returnPrimIO a #-}
returnPrimIO    :: a -> PrimIO a

{-# GENERATE_SPECS thenPrimIO b #-}
thenPrimIO      :: PrimIO a -> (a -> PrimIO b) -> PrimIO b

{-# GENERATE_SPECS seqPrimIO b #-}
seqPrimIO	:: PrimIO a -> PrimIO b -> PrimIO b

listPrimIO	:: [PrimIO a] -> PrimIO [a]
mapPrimIO	:: (a -> PrimIO b) -> [a] -> PrimIO [b]
mapAndUnzipPrimIO :: (a -> PrimIO (b,c)) -> [a] -> PrimIO ([b],[c])

{-# INLINE returnPrimIO #-}
{-# INLINE thenPrimIO   #-}
{-# INLINE seqPrimIO  #-}

returnPrimIO  	  = return
thenPrimIO    	  = (>>=)
seqPrimIO     	  = (>>)
listPrimIO    	  = accumulate
mapPrimIO     	  = mapM
mapAndUnzipPrimIO = mapAndUnzipM
\end{code}


%*********************************************************
%*							*
\subsection{Ghastly return types}
%*							*
%*********************************************************

The @State@ type is the return type of a _ccall_ with no result.  It
never actually exists, since it's always deconstructed straight away;
the desugarer ensures this.

\begin{code}
data State	     s     = S#		     (State# s)
data StateAndPtr#    s elt = StateAndPtr#    (State# s) elt 

data StateAndChar#   s     = StateAndChar#   (State# s) Char# 
data StateAndInt#    s     = StateAndInt#    (State# s) Int# 
data StateAndWord#   s     = StateAndWord#   (State# s) Word#
data StateAndFloat#  s     = StateAndFloat#  (State# s) Float# 
data StateAndDouble# s     = StateAndDouble# (State# s) Double#  
data StateAndAddr#   s     = StateAndAddr#   (State# s) Addr#
\end{code}
