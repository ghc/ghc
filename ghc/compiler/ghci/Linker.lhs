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
   resolveObjs   -- :: IO Bool
  )  where

import Monad            ( when )

import Foreign.C
import Foreign		( Ptr, nullPtr )
import Panic		( panic )
import DriverUtil       ( prefixUnderscore )

-- ---------------------------------------------------------------------------
-- RTS Linker Interface
-- ---------------------------------------------------------------------------

lookupSymbol :: String -> IO (Maybe (Ptr a))
lookupSymbol str_in = do
   let str = prefixUnderscore str_in
   withCString str $ \c_str -> do
     addr <- c_lookupSymbol c_str
     if addr == nullPtr
	then return Nothing
	else return (Just addr)

loadObj :: String -> IO ()
loadObj str =
   withCString str $ \c_str -> do
     r <- c_loadObj c_str
     when (r == 0) (panic "loadObj: failed")

unloadObj :: String -> IO ()
unloadObj str =
   withCString str $ \c_str -> do
     r <- c_unloadObj c_str
     when (r == 0) (panic "unloadObj: failed")

resolveObjs :: IO Bool
resolveObjs = do
   r <- c_resolveObjs
   return (r /= 0)  -- returns True <=> success

-- ---------------------------------------------------------------------------
-- Foreign declaractions to RTS entry points which does the real work;
-- ---------------------------------------------------------------------------

foreign import "initLinker" unsafe
   initLinker :: IO ()

foreign import "lookupSymbol" unsafe
   c_lookupSymbol :: CString -> IO (Ptr a)

foreign import "loadObj" unsafe
   c_loadObj :: CString -> IO Int

foreign import "unloadObj" unsafe
   c_unloadObj :: CString -> IO Int

foreign import "resolveObjs" unsafe
   c_resolveObjs :: IO Int

\end{code}
