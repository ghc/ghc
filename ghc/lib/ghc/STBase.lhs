%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[STBase]{The @ST@ and @PrimIO@ monads}

\begin{code}
module STBase where

import Prelude ()
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
data State a   = S# (State# a)
newtype ST s a = ST (State s -> (a, State s))

runST (ST m)
  = case m (S# realWorld#) of
      (r,_) -> r

instance Monad (ST s) where
    {-# INLINE return #-}
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    return x = ST $ \ s@(S# _) -> (x, s)
    m >> k   =  m >>= \ _ -> k

    (ST m) >>= k
      = ST $ \ s ->
	case (m s) of {(r, new_s) ->
	case (k r) of { ST k2 ->
	(k2 new_s) }}

{-# INLINE returnST #-}

-- here for backward compatibility:
returnST :: a -> ST s a
thenST	 :: ST s a -> (a -> ST s b) -> ST s b
seqST	 :: ST s a -> ST s b -> ST s b

returnST = return
thenST   = (>>=)
seqST	 = (>>)

-- not sure whether to 1.3-ize these or what...
{-# INLINE returnStrictlyST #-}
{-# INLINE thenStrictlyST #-}
{-# INLINE seqStrictlyST #-}

{-# GENERATE_SPECS returnStrictlyST a #-}
returnStrictlyST :: a -> ST s a

{-# GENERATE_SPECS thenStrictlyST a b #-}
thenStrictlyST :: ST s a -> (a -> ST s b) -> ST s b

{-# GENERATE_SPECS seqStrictlyST a b #-}
seqStrictlyST :: ST s a -> ST s b -> ST s b

returnStrictlyST a = ST $ \ s@(S# _) -> (a, s)

thenStrictlyST (ST m) k = ST $ \ s ->	-- @(S# _)   Omitted SLPJ [May95] no need to evaluate the state
    case (m s) of { (r, new_s) ->
    case (k r) of { ST k2     ->
    (k2 new_s) }}

seqStrictlyST (ST m) (ST k) = ST $ \ s ->	-- @(S# _)   Omitted SLPJ [May95] no need to evaluate the state
    case (m s) of { (_, new_s) ->
    (k new_s) }

-- BUILT-IN: runST (see Builtin.hs)

unsafeInterleaveST :: ST s a -> ST s a    -- ToDo: put in state-interface.tex
unsafeInterleaveST (ST m) = ST $ \ s ->
    let
	(r, new_s) = m s
    in
    (r, s)

fixST :: (a -> ST s a) -> ST s a
fixST k = ST $ \ s ->
    let (ST k_r)  = k r
	ans       = k_r s
	(r,new_s) = ans
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

{-# GENERATE_SPECS unsafePerformPrimIO a #-}
unsafePerformPrimIO	:: PrimIO a -> a
unsafeInterleavePrimIO	:: PrimIO a -> PrimIO a

unsafePerformPrimIO	= runST
unsafeInterleavePrimIO	= unsafeInterleaveST

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

\begin{code}
data StateAndPtr#    s elt = StateAndPtr#    (State# s) elt 

data StateAndChar#   s     = StateAndChar#   (State# s) Char# 
data StateAndInt#    s     = StateAndInt#    (State# s) Int# 
data StateAndWord#   s     = StateAndWord#   (State# s) Word#
data StateAndFloat#  s     = StateAndFloat#  (State# s) Float# 
data StateAndDouble# s     = StateAndDouble# (State# s) Double#  
data StateAndAddr#   s     = StateAndAddr#   (State# s) Addr#
\end{code}
