-----------------------------------------------------------------------------
-- |
-- Module      :  System.Environment
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Miscellaneous information about the system environment.
--
-----------------------------------------------------------------------------

module System.Environment
    ( 
    , getArgs	    -- :: IO [String]
    , getProgName   -- :: IO String
    , getEnv        -- :: String -> IO String
  ) where

import Prelude

import Foreign
import Foreign.C
import Control.Monad

#ifdef __GLASGOW_HASKELL__
import GHC.IOBase
#endif

-- ---------------------------------------------------------------------------
-- getArgs, getProgName, getEnv

-- Computation `getArgs' returns a list of the program's command
-- line arguments (not including the program name).

getArgs :: IO [String]
getArgs = 
  alloca $ \ p_argc ->  
  alloca $ \ p_argv -> do
   getProgArgv p_argc p_argv
   p    <- fromIntegral `liftM` peek p_argc
   argv <- peek p_argv
   peekArray (p - 1) (advancePtr argv 1) >>= mapM peekCString

   
foreign import ccall unsafe "getProgArgv"
  getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

{-|
Computation 'getProgName' returns the name of the program as it was
invoked.

However, this is hard-to-impossible to implement on some non-Unix
OSes, so instead, for maximum portability, we just return the leafname
of the program as invoked. Even then there are some differences
between platforms: on Windows, for example, a program invoked as foo
is probably really @FOO.EXE@, and that is what 'getProgName' will return.
-}
getProgName :: IO String
getProgName = 
  alloca $ \ p_argc ->
  alloca $ \ p_argv -> do
     getProgArgv p_argc p_argv
     argv <- peek p_argv
     unpackProgName argv
  
unpackProgName	:: Ptr (Ptr CChar) -> IO String   -- argv[0]
unpackProgName argv = do 
  s <- peekElemOff argv 0 >>= peekCString
  return (basename s)
  where
   basename :: String -> String
   basename f = go f f
    where
      go acc [] = acc
      go acc (x:xs) 
        | isPathSeparator x = go xs xs
        | otherwise         = go acc xs

   isPathSeparator :: Char -> Bool
   isPathSeparator '/'  = True
#ifdef mingw32_TARGET_OS 
   isPathSeparator '\\' = True
#endif
   isPathSeparator _    = False


-- Computation `getEnv var' returns the value
-- of the environment variable {\em var}.  

-- This computation may fail with
--    NoSuchThing: The environment variable does not exist.

getEnv :: String -> IO String
getEnv name =
    withCString name $ \s -> do
      litstring <- c_getenv s
      if litstring /= nullPtr
	then peekCString litstring
        else ioException (IOError Nothing NoSuchThing "getEnv"
			  "no environment variable" (Just name))

foreign import ccall unsafe "getenv"
   c_getenv :: CString -> IO (Ptr CChar)
