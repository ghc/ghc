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
	makeForeignObj,
	writeForeignObj
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
--instance CCallable ForeignObj
--instance CCallable ForeignObj#

makeForeignObj  :: Addr -> IO ForeignObj
makeForeignObj (A# obj) = IO ( \ s# ->
    case makeForeignObj# obj s# of
      (# s1#, fo# #) -> (# s1#,  ForeignObj fo# #) )

eqForeignObj    :: ForeignObj  -> ForeignObj -> Bool
--makeForeignObj  :: Addr        -> Addr       -> IO ForeignObj
writeForeignObj :: ForeignObj  -> Addr       -> IO ()

writeForeignObj (ForeignObj fo#) (A# datum#) = IO ( \ s# ->
    case writeForeignObj# fo# datum# s# of { s1# -> (# s1#, () #) } )

eqForeignObj mp1 mp2
  = unsafePerformIO (primEqForeignObj mp1 mp2) /= (0::Int)

foreign import "eqForeignObj" unsafe primEqForeignObj :: ForeignObj -> ForeignObj -> IO Int

instance Eq ForeignObj where 
    p == q = eqForeignObj p q
    p /= q = not (eqForeignObj p q)
#endif /* !__PARALLEL_HASKELL__ */
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
