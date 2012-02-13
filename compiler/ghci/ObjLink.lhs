%
% (c) The University of Glasgow, 2000-2006
%

-- ---------------------------------------------------------------------------
-- 	The dynamic linker for object code (.o .so .dll files)
-- ---------------------------------------------------------------------------

Primarily, this module consists of an interface to the C-land dynamic linker.

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module ObjLink ( 
   initObjLinker,	 -- :: IO ()
   loadDLL,		 -- :: String -> IO (Maybe String)
   loadArchive,     	 -- :: String -> IO ()
   loadObj,     	 -- :: String -> IO ()
   unloadObj,   	 -- :: String -> IO ()
   insertSymbol,         -- :: String -> String -> Ptr a -> IO ()
   lookupSymbol,	 -- :: String -> IO (Maybe (Ptr a))
   resolveObjs  	 -- :: IO SuccessFlag
  )  where

import Panic
import BasicTypes	( SuccessFlag, successIf )
import Config		( cLeadingUnderscore )
import Util

import Control.Monad    ( when )
import Foreign.C
import Foreign		( nullPtr )
import GHC.Exts         ( Ptr(..) )
import System.Posix.Internals ( CFilePath, withFilePath )
import System.FilePath  ( dropExtension )


-- ---------------------------------------------------------------------------
-- RTS Linker Interface
-- ---------------------------------------------------------------------------

insertSymbol :: String -> String -> Ptr a -> IO ()
insertSymbol obj_name key symbol
    = let str = prefixUnderscore key
      in withFilePath obj_name $ \c_obj_name ->
         withCAString str $ \c_str ->
          c_insertSymbol c_obj_name c_str symbol

lookupSymbol :: String -> IO (Maybe (Ptr a))
lookupSymbol str_in = do
   let str = prefixUnderscore str_in
   withCAString str $ \c_str -> do
     addr <- c_lookupSymbol c_str
     if addr == nullPtr
	then return Nothing
	else return (Just addr)

prefixUnderscore :: String -> String
prefixUnderscore
 | cLeadingUnderscore == "YES" = ('_':)
 | otherwise                   = id

-- | loadDLL loads a dynamic library using the OS's native linker
-- (i.e. dlopen() on Unix, LoadLibrary() on Windows).  It takes either
-- an absolute pathname to the file, or a relative filename
-- (e.g. "libfoo.so" or "foo.dll").  In the latter case, loadDLL
-- searches the standard locations for the appropriate library.
--
loadDLL :: String -> IO (Maybe String)
-- Nothing      => success
-- Just err_msg => failure
loadDLL str0 = do
  let
     -- On Windows, addDLL takes a filename without an extension, because
     -- it tries adding both .dll and .drv.  To keep things uniform in the
     -- layers above, loadDLL always takes a filename with an extension, and
     -- we drop it here on Windows only.
     str | isWindowsHost = dropExtension str0
         | otherwise     = str0
  --
  maybe_errmsg <- withFilePath str $ \dll -> c_addDLL dll
  if maybe_errmsg == nullPtr
	then return Nothing
	else do str <- peekCString maybe_errmsg
		return (Just str)

loadArchive :: String -> IO ()
loadArchive str = do
   withFilePath str $ \c_str -> do
     r <- c_loadArchive c_str
     when (r == 0) (panic ("loadArchive " ++ show str ++ ": failed"))

loadObj :: String -> IO ()
loadObj str = do
   withFilePath str $ \c_str -> do
     r <- c_loadObj c_str
     when (r == 0) (panic ("loadObj " ++ show str ++ ": failed"))

unloadObj :: String -> IO ()
unloadObj str =
   withFilePath str $ \c_str -> do
     r <- c_unloadObj c_str
     when (r == 0) (panic ("unloadObj " ++ show str ++ ": failed"))

resolveObjs :: IO SuccessFlag
resolveObjs = do
   r <- c_resolveObjs
   return (successIf (r /= 0))

-- ---------------------------------------------------------------------------
-- Foreign declarations to RTS entry points which does the real work;
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "addDLL"       c_addDLL :: CFilePath -> IO CString
foreign import ccall unsafe "initLinker"   initObjLinker :: IO ()
foreign import ccall unsafe "insertSymbol" c_insertSymbol :: CFilePath -> CString -> Ptr a -> IO ()
foreign import ccall unsafe "lookupSymbol" c_lookupSymbol :: CString -> IO (Ptr a)
foreign import ccall unsafe "loadArchive"  c_loadArchive :: CFilePath -> IO Int
foreign import ccall unsafe "loadObj"      c_loadObj :: CFilePath -> IO Int
foreign import ccall unsafe "unloadObj"    c_unloadObj :: CFilePath -> IO Int
foreign import ccall unsafe "resolveObjs"  c_resolveObjs :: IO Int
\end{code}
