%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[md5]{MD5: Message-digest}

This module provides basic MD5 support for Haskell, using
Colin Plumb's C implementation of MD5 to do the Hard Work.

\begin{code}
{-# OPTIONS -#include "cbits/md5.h" #-}
module MD5 
	(
          digest	-- :: String	   -> IO String
	, digestPS      -- :: PackedString -> IO (ByteArray Int)
	) where


import GlaExts
import Addr
import PackedString

\end{code}

\begin{code}
digest :: String -> IO String
digest str = do
  ps   <- stToIO (packStringST str)
  ba   <- digestPS ps
  let (ByteArray _ ba#) = ba
  baToString ba# 16# 0#
 where
  baToString ba# n# i#
    | n# ==# 0# = return []
    | otherwise = do
       let ch# = indexCharArray# ba# i#
       ls <- baToString ba# (n# -# 1#) (i# +# 1#)
       return ((C# ch#):ls)

digestPS :: PackedString -> IO (ByteArray Int)
digestPS ps = do
  ctxt <- stToIO (newCharArray (0::Int,``sizeof(struct MD5Context)''::Int))
  let len = lengthPS ps
  _ccall_ MD5Init ctxt
  (if isCString ps
    then _ccall_ MD5Update ctxt (psToCString ps) len
    else _ccall_ MD5Update ctxt (psToByteArray ps) len)
  dig  <- stToIO (newCharArray (0::Int,16*(``sizeof(unsigned char)''::Int)))
  _ccall_ MD5Final dig ctxt
  stToIO (unsafeFreezeByteArray dig)

\end{code}
