%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[Foreign]{Module @Foreign@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module Foreign (
	module Foreign,
#ifndef __PARALLEL_HASKELL__
	ForeignObj(..),
#endif
	Addr(..), Word(..)
   ) where

import STBase
import UnsafeST	( unsafePerformPrimIO )
import PrelBase
import GHC
\end{code}


%*********************************************************
%*							*
\subsection{Classes @CCallable@ and @CReturnable@}
%*							*
%*********************************************************

\begin{code}
class CCallable   a
class CReturnable a

instance CCallable Char
instance CCallable   Char#
instance CReturnable Char

instance CCallable   Int
instance CCallable   Int#
instance CReturnable Int

-- DsCCall knows how to pass strings...
instance CCallable   [Char]

instance CCallable   Float
instance CCallable   Float#
instance CReturnable Float

instance CCallable   Double
instance CCallable   Double#
instance CReturnable Double

data Addr = A# Addr# 	deriving (Eq, Ord) -- Glasgow extension

instance CCallable Addr
instance CCallable Addr#
instance CReturnable Addr

data Word = W# Word# 	deriving (Eq, Ord) -- Glasgow extension

instance CCallable Word
instance CCallable Word#
instance CReturnable Word

instance CReturnable () -- Why, exactly?
\end{code}


%*********************************************************
%*							*
\subsection{Type @ForeignObj@ and its operations}
%*							*
%*********************************************************

\begin{code}
#ifndef __PARALLEL_HASKELL__
--Defined in PrelBase: data ForeignObj = ForeignObj ForeignObj#
data ForeignObj = ForeignObj ForeignObj#   -- another one

instance CCallable ForeignObj
instance CCallable ForeignObj#

eqForeignObj    :: ForeignObj  -> ForeignObj -> Bool
makeForeignObj  :: Addr        -> Addr       -> PrimIO ForeignObj
writeForeignObj :: ForeignObj  -> Addr       -> PrimIO ()

{- derived op - attaching a free() finaliser to a malloc() allocated reference. -}
makeMallocPtr   :: Addr        -> PrimIO ForeignObj

makeForeignObj (A# obj) (A# finaliser) = ST ( \ s# ->
    case makeForeignObj# obj finaliser s# of
      StateAndForeignObj# s1# fo# -> STret s1# (ForeignObj fo#))

writeForeignObj (ForeignObj fo#) (A# datum#) = ST ( \ s# ->
    case writeForeignObj# fo# datum# s# of { s1# -> STret s1# () } )

makeMallocPtr a = makeForeignObj a (``&free''::Addr)

eqForeignObj mp1 mp2
  = unsafePerformPrimIO (_ccall_ eqForeignObj mp1 mp2) /= (0::Int)

instance Eq ForeignObj where 
    p == q = eqForeignObj p q
    p /= q = not (eqForeignObj p q)
#endif /* !__PARALLEL_HASKELL__ */
\end{code}


%*********************************************************
%*							*
\subsection{Type @StablePtr@ and its operations}
%*							*
%*********************************************************

\begin{code}
#ifndef __PARALLEL_HASKELL__
data StablePtr a = StablePtr (StablePtr# a)
instance CCallable   (StablePtr a)
instance CCallable   (StablePtr# a)
instance CReturnable (StablePtr a)

-- Nota Bene: it is important {\em not\/} to inline calls to
-- @makeStablePtr#@ since the corresponding macro is very long and we'll
-- get terrible code-bloat.

makeStablePtr  :: a -> PrimIO (StablePtr a)
deRefStablePtr :: StablePtr a -> PrimIO a
freeStablePtr  :: StablePtr a -> PrimIO ()
performGC      :: PrimIO ()

{-# INLINE deRefStablePtr #-}
{-# INLINE freeStablePtr #-}
{-# INLINE performGC #-}

makeStablePtr f = ST $ \ rw1# ->
    case makeStablePtr# f rw1# of
      StateAndStablePtr# rw2# sp# -> STret rw2# (StablePtr sp#)

deRefStablePtr (StablePtr sp#) = ST $ \ rw1# ->
    case deRefStablePtr# sp# rw1# of
      StateAndPtr# rw2# a -> STret rw2# a

freeStablePtr sp = _ccall_ freeStablePointer sp

performGC = _ccall_GC_ StgPerformGarbageCollection

#endif /* !__PARALLEL_HASKELL__ */
\end{code}

%*********************************************************
%*							*
\subsection{Ghastly return types}
%*							*
%*********************************************************

\begin{code}
#ifndef __PARALLEL_HASKELL__
data StateAndStablePtr# s a = StateAndStablePtr# (State# s) (StablePtr# a)
#endif
data StateAndForeignObj# s  = StateAndForeignObj# (State# s) ForeignObj#
\end{code}
