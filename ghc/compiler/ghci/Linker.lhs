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

import PrelByteArr
import PrelPack 	( packString )

import Monad            ( when )

import CTypes		( CChar )
import Foreign		( Ptr, nullPtr )
import Panic		( panic )
import DriverUtil       ( prefixUnderscore )

-- ---------------------------------------------------------------------------
-- RTS Linker Interface
-- ---------------------------------------------------------------------------

lookupSymbol :: String -> IO (Maybe (Ptr a))
lookupSymbol str_in = do
   let str = prefixUnderscore str_in
   addr <- c_lookupSymbol (packString str)
   if addr == nullPtr
	then return Nothing
	else return (Just addr)

loadObj :: String -> IO ()
loadObj str = do
   r <- c_loadObj (packString str)
   when (r == 0) (panic "loadObj: failed")

unloadObj :: String -> IO ()
unloadObj str = do
   r <- c_unloadObj (packString str)
   when (r == 0) (panic "unloadObj: failed")

resolveObjs :: IO Bool
resolveObjs = do
   r <- c_resolveObjs
   return (r /= 0)  -- returns True <=> success

addDLL :: String -> String -> IO (Ptr CChar)
addDLL path lib = do
   maybe_errmsg <- c_addDLL (packString path) (packString lib)
   return maybe_errmsg


foreign import "initLinker" unsafe
   initLinker :: IO ()

-- ---------------------------------------------------------------------------
-- Foreign declaractions to RTS entry points which does the real work;
-- ---------------------------------------------------------------------------

type PackedString = ByteArray Int

foreign import "lookupSymbol" unsafe
   c_lookupSymbol :: PackedString -> IO (Ptr a)

foreign import "loadObj" unsafe
   c_loadObj :: PackedString -> IO Int

foreign import "unloadObj" unsafe
   c_unloadObj :: PackedString -> IO Int

foreign import "resolveObjs" unsafe
   c_resolveObjs :: IO Int

foreign import "addDLL" unsafe 
   c_addDLL :: PackedString -> PackedString -> IO (Ptr CChar)
\end{code}
