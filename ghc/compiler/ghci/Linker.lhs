%
% (c) The University of Glasgow, 2000
%
\section[Linker]{The In-Memory Object File Linker}

\begin{code}
{-# OPTIONS -#include "Linker.h" #-}
module Linker ( 
   loadObj,      -- :: String -> IO ()
   unloadObj,    -- :: String -> IO ()
   lookupSymbol, -- :: String -> IO (Maybe Addr)
   resolveObjs,  -- :: IO ()
   linkPrelude -- tmp
  )  where

import IO
import Exception
import Addr
import PrelByteArr
import PrelPack 	(packString)
import Panic		( panic )

#if __GLASGOW_HASKELL__ <= 408
loadObj      = bogus "loadObj"
unloadObj    = bogus "unloadObj"
lookupSymbol = bogus "lookupSymbol"
resolveObjs  = bogus "resolveObjs"
linkPrelude  = bogus "linkPrelude"
bogus f = panic ("Linker." ++ f ++ ": this hsc was built without an interpreter.")

#else

linkPrelude = do
  hPutStr stderr "Loading HSstd_cbits.o..."
  loadObj "/home/simonmar/builds/i386-unknown-linux-boot/ghc/lib/std/cbits/HSstd_cbits.o"
  hPutStr stderr "done.\n"
  hPutStr stderr "Resolving..."
  resolveObjs
  hPutStr stderr "done.\n"
  hPutStr stderr "Loading HSstd.o..."
  loadObj "/home/simonmar/builds/i386-unknown-linux-boot/ghc/lib/std/HSstd.o"
  hPutStr stderr "done.\n"
  hPutStr stderr "Resolving..."
  resolveObjs
  hPutStr stderr "done.\n"
{-
  hPutStr stderr "Unloading HSstd.o..."
  unloadObj "/home/simonmar/builds/i386-unknown-linux-boot/ghc/lib/std/HSstd.o"
  hPutStr stderr "done.\n"
  unloadObj "/home/simonmar/builds/i386-unknown-linux-boot/ghc/lib/std/cbits/HSstd_cbits.o"
  hPutStr stderr "done.\n"
-}

-- ---------------------------------------------------------------------------
-- RTS Linker Interface
-- ---------------------------------------------------------------------------

lookupSymbol str = do
   addr <- c_lookupSymbol (packString str)
   if addr == nullAddr
	then return Nothing
	else return (Just addr)

loadObj str = do
   r <- c_loadObj (packString str)
   if (r == 0)
	then error "loadObj: failed"
	else return ()

unloadObj str = do
   r <- c_unloadObj (packString str)
   if (r == 0)
	then error "unloadObj: failed"
	else return ()

resolveObjs = do
   r <- c_resolveObjs
   if (r == 0)
	then error "resolveObjs: failed"
	else return ()


type PackedString = ByteArray Int

foreign import "lookupSymbol" unsafe
   c_lookupSymbol :: PackedString -> IO Addr

foreign import "loadObj" unsafe
   c_loadObj :: PackedString -> IO Int

foreign import "unloadObj" unsafe
   c_unloadObj :: PackedString -> IO Int

foreign import "resolveObjs" unsafe
   c_resolveObjs :: IO Int

#endif /* __GLASGOW_HASKELL__ <= 408 */
\end{code}
