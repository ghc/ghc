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

addDLL :: String -> String -> IO (Ptr CChar)
addDLL path lib = do
  withCString path $ \c_path -> do
  withCString lib $ \c_lib -> do
    maybe_errmsg <- c_addDLL c_path c_lib
    return maybe_errmsg

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

foreign import "addDLL" unsafe 
   c_addDLL :: CString -> CString -> IO (Ptr CChar)
\end{code}
