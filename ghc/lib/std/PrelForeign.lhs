%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[Foreign]{Module @Foreign@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelForeign (
	module PrelForeign,
#ifndef __PARALLEL_HASKELL__
	ForeignObj(..),
#endif
	Word(..),

#ifndef __PARALLEL_HASKELL__
	unpackCStringFO,   -- :: ForeignObj    -> [Char]
	unpackNBytesFO,    -- :: ForeignObj    -> Int  -> [Char]
	unpackCStringFO#,  -- :: ForeignObj#   -> [Char]
	unpackNBytesFO#    -- :: ForeignObj#   -> Int# -> [Char]
#endif
   ) where

import PrelIOBase
import PrelST
import PrelBase
import PrelCCall
import PrelAddr
import PrelGHC
\end{code}


%*********************************************************
%*							*
\subsection{Type @ForeignObj@ and its operations}
%*							*
%*********************************************************

\begin{code}
#ifndef __PARALLEL_HASKELL__
instance CCallable ForeignObj
instance CCallable ForeignObj#

eqForeignObj    :: ForeignObj  -> ForeignObj -> Bool
makeForeignObj  :: Addr        -> Addr       -> IO ForeignObj
writeForeignObj :: ForeignObj  -> Addr       -> IO ()

{- derived op - attaching a free() finaliser to a malloc() allocated reference. -}
makeMallocPtr   :: Addr        -> IO ForeignObj

makeForeignObj (A# obj) (A# finaliser) = IO ( \ s# ->
    case makeForeignObj# obj finaliser s# of
      StateAndForeignObj# s1# fo# -> IOok s1# (ForeignObj fo#))

writeForeignObj (ForeignObj fo#) (A# datum#) = IO ( \ s# ->
    case writeForeignObj# fo# datum# s# of { s1# -> IOok s1# () } )

makeMallocPtr a = makeForeignObj a (``&free''::Addr)

eqForeignObj mp1 mp2
  = unsafePerformIO (_ccall_ eqForeignObj mp1 mp2) /= (0::Int)

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

makeStablePtr  :: a -> IO (StablePtr a)
deRefStablePtr :: StablePtr a -> IO a
freeStablePtr  :: StablePtr a -> IO ()

{-# INLINE deRefStablePtr #-}
{-# INLINE freeStablePtr #-}

makeStablePtr f = IO $ \ rw1# ->
    case makeStablePtr# f rw1# of
      StateAndStablePtr# rw2# sp# -> IOok rw2# (StablePtr sp#)

deRefStablePtr (StablePtr sp#) = IO $ \ rw1# ->
    case deRefStablePtr# sp# rw1# of
      StateAndPtr# rw2# a -> IOok rw2# a

freeStablePtr sp = _ccall_ freeStablePointer sp

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

%*********************************************************
%*							*
\subsection{Unpacking Foreigns}
%*							*
%*********************************************************

Primitives for converting Foreigns pointing to external
sequence of bytes into a list of @Char@s (a renamed version
of the code above).

\begin{code}
#ifndef __PARALLEL_HASKELL__
unpackCStringFO :: ForeignObj -> [Char]
unpackCStringFO (ForeignObj fo#) = unpackCStringFO# fo#

unpackCStringFO# :: ForeignObj# -> [Char]
unpackCStringFO# fo {- ptr. to NUL terminated string-}
  = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'# = []
      | otherwise	   = C# ch : unpack (nh +# 1#)
      where
	ch = indexCharOffForeignObj# fo nh

unpackNBytesFO :: ForeignObj -> Int -> [Char]
unpackNBytesFO (ForeignObj fo) (I# l) = unpackNBytesFO# fo l

unpackNBytesFO#    :: ForeignObj# -> Int#   -> [Char]
  -- This one is called by the compiler to unpack literal strings with NULs in them; rare.
unpackNBytesFO# fo len
  = unpack 0#
    where
     unpack i
      | i >=# len  = []
      | otherwise  = C# ch : unpack (i +# 1#)
      where
	ch = indexCharOffForeignObj# fo i
#endif
\end{code}


