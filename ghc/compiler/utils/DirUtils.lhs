%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[DirUtils]{Directory functions}

\begin{code}
{-# OPTIONS -#include <dirent.h> #-}
module DirUtils
       (
	getDirectoryContents
       ) where

#if !defined(__GLASGOW_HASKELL__) || __GLASGOW_HASKELL__ >= 302
import Directory
#else

#if __GLASGOW_HASKELL__ >= 300
import PrelPack   ( unpackNBytesST )
#else
import PackBase   ( unpackNBytesST )
#endif
import PrimPacked ( strLength )
import GlaExts    ( stToIO )
import Addr	  ( Addr )

\end{code}

The implementation of Directory.getDirectoryContents that ships
with ghc-X ( X<=301) is wrong (the C stub fails to allocate
space for the terminating NUL for each directory entry name.)

To counter for this, we supply a working version here, which will
be nuked once we can assume that ghc-3.02 or later is used to 
compile the compiler sources.

\begin{code}
getDirectoryContents :: String -> IO [String]
getDirectoryContents path = do
    dir <- _ccall_ opendir path
    if dir == ``NULL'' 
	then fail (userError ("DirUtils.getDirectoryContents: couldn't open "++ path))
     	else loop dir
  where
    loop :: Addr -> IO [String]
    loop dir  = do
      dirent_ptr <- _ccall_ readdir dir
      if (dirent_ptr::Addr) == ``NULL'' 
       then do
          _ccall_ closedir dir
	  return [] 
       else do
          str     <- _casm_ `` %r=(char*)((struct dirent*)%0)->d_name; '' dirent_ptr
	  entry   <- stToIO (unpackNBytesST str (strLength str))
          entries <- loop dir
          return (entry:entries)
#endif
\end{code}
