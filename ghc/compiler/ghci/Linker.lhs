%
% (c) The University of Glasgow, 2000
%
\section[Linker]{The In-Memory Object File Linker}

\begin{code}
{-# OPTIONS -#include "Linker.h" #-}

-- so that we can see defn of LEADING_UNDERSCORE
#include "../includes/config.h"

module Linker ( 
   initLinker,	 -- :: IO ()
   loadObj,      -- :: String -> IO ()
   unloadObj,    -- :: String -> IO ()
   lookupSymbol, -- :: String -> IO (Maybe (Ptr a))
   resolveObjs,  -- :: IO ()
   addDLL	 -- :: String -> IO (Ptr CChar)
  )  where

import CTypes		( CChar )
import Foreign		( Ptr, nullPtr )
import PrelByteArr
import PrelPack 	(packString)
import Panic		( panic )

-- ---------------------------------------------------------------------------
-- RTS Linker Interface
-- ---------------------------------------------------------------------------

lookupSymbol str_in = do
#  ifdef LEADING_UNDERSCORE
   let str = '_':str_in
#  else
   let str = str_in
#  endif
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
   if (r == 0)
	then panic "resolveObjs: failed"
	else return ()

addDLL str = do
   maybe_errmsg <- c_addDLL (packString str)
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
   c_addDLL :: PackedString -> IO (Ptr CChar)

\end{code}
