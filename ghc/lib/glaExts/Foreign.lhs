%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[Foreign]{Module @Foreign@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module Foreign (
	module Foreign,
	ForeignObj(..),
	Addr, Word
   ) where

import STBase
import ArrBase
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

instance CCallable Addr
instance CCallable Addr#
instance CReturnable Addr

instance CCallable Word
instance CCallable Word#
instance CReturnable Word

-- Is this right?
instance CCallable (MutableByteArray s ix)
instance CCallable (MutableByteArray# s)

instance CCallable (ByteArray ix)
instance CCallable ByteArray#

instance CReturnable () -- Why, exactly?
\end{code}


%*********************************************************
%*							*
\subsection{Type @ForeignObj@ and its operations}
%*							*
%*********************************************************

\begin{code}
--Defined in PrelBase: data ForeignObj = ForeignObj ForeignObj#
instance CCallable ForeignObj
instance CCallable ForeignObj#

eqForeignObj    :: ForeignObj  -> ForeignObj -> Bool
makeForeignObj  :: Addr        -> Addr       -> PrimIO ForeignObj
writeForeignObj :: ForeignObj  -> Addr       -> PrimIO ()

{- derived op - attaching a free() finaliser to a malloc() allocated reference. -}
makeMallocPtr   :: Addr        -> PrimIO ForeignObj

makeForeignObj (A# obj) (A# finaliser) = ST ( \ (S# s#) ->
    case makeForeignObj# obj finaliser s# of
      StateAndForeignObj# s1# fo# -> (ForeignObj fo#, S# s1#))

writeForeignObj (ForeignObj fo#) (A# datum#) = ST ( \ (S# s#) ->
    case writeForeignObj# fo# datum# s# of { s1# -> ((), S# s1#) } )

makeMallocPtr a = makeForeignObj a (``&free''::Addr)

eqForeignObj mp1 mp2
  = unsafePerformPrimIO (_ccall_ eqForeignObj mp1 mp2) /= (0::Int)

instance Eq ForeignObj where 
    p == q = eqForeignObj p q
    p /= q = not (eqForeignObj p q)
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

makeStablePtr f = ST $ \ (S# rw1#) ->
    case makeStablePtr# f rw1# of
      StateAndStablePtr# rw2# sp# -> (StablePtr sp#, S# rw2#)

deRefStablePtr (StablePtr sp#) = ST $ \ (S# rw1#) ->
    case deRefStablePtr# sp# rw1# of
      StateAndPtr# rw2# a -> (a, S# rw2#)

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
