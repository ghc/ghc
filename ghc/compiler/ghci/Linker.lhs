%
% (c) The University of Glasgow, 2000
%
\section[Linker]{The In-Memory Object File Linker}

\begin{code}
{-# OPTIONS -#include "Linker.h" #-}

module Linker ( 
   initLinker,	 -- :: IO ()
   loadObj,      -- :: String -> IO ()
   unloadObj,    -- :: String -> IO ()
   lookupSymbol, -- :: String -> IO (Maybe (Ptr a))
   resolveObjs,  -- :: IO Bool
   addDLL	 -- :: String -> IO (Ptr CChar)
  )  where

import CTypes		( CChar )
import Foreign		( Ptr, nullPtr )
import PrelByteArr
import PrelPack 	(packString)
import Panic		( panic )
import DriverUtil       ( prefixUnderscore )

-- ---------------------------------------------------------------------------
-- RTS Linker Interface
-- ---------------------------------------------------------------------------

lookupSymbol str_in = do
   let str = prefixUnderscore str_in
   addr <- c_lookupSymbol (packString str)
   if addr == nullPtr
	then return Nothing
	else return (Just addr)

loadObj str = do
   r <- c_loadObj (packString str)
   if (r == 0)
	then panic "loadObj: failed"
	else return ()

unloadObj str = do
   r <- c_unloadObj (packString str)
   if (r == 0)
	then panic "unloadObj: failed"
	else return ()

resolveObjs = do
   r <- c_resolveObjs
   return (r /= 0)  -- returns True <=> success

addDLL path lib = do
   maybe_errmsg <- c_addDLL (packString path) (packString lib)
   return maybe_errmsg

type PackedString = ByteArray Int

foreign import "lookupSymbol" unsafe
   c_lookupSymbol :: PackedString -> IO (Ptr a)

foreign import "loadObj" unsafe
   c_loadObj :: PackedString -> IO Int

foreign import "unloadObj" unsafe
   c_unloadObj :: PackedString -> IO Int

foreign import "resolveObjs" unsafe
   c_resolveObjs :: IO Int

foreign import "initLinker" unsafe
   initLinker :: IO ()

foreign import "addDLL" unsafe 
   c_addDLL :: PackedString -> PackedString -> IO (Ptr CChar)
\end{code}
