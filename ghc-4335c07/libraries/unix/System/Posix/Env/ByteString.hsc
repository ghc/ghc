{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE Trustworthy #-}
#if __GLASGOW_HASKELL__ >= 709
{-# OPTIONS_GHC -fno-warn-trustworthy-safe #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Env.ByteString
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX environment support
--
-----------------------------------------------------------------------------

module System.Posix.Env.ByteString (
       -- * Environment Variables
        getEnv
        , getEnvDefault
        , getEnvironmentPrim
        , getEnvironment
        , putEnv
        , setEnv
       , unsetEnv

       -- * Program arguments
       , getArgs
) where

#include "HsUnix.h"

import Foreign
import Foreign.C
import Control.Monad    ( liftM )
import Data.Maybe       ( fromMaybe )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)

-- |'getEnv' looks up a variable in the environment.

getEnv ::
  ByteString            {- ^ variable name  -} ->
  IO (Maybe ByteString) {- ^ variable value -}
getEnv name = do
  litstring <- B.useAsCString name c_getenv
  if litstring /= nullPtr
     then liftM Just $ B.packCString litstring
     else return Nothing

-- |'getEnvDefault' is a wrapper around 'getEnv' where the
-- programmer can specify a fallback if the variable is not found
-- in the environment.

getEnvDefault ::
  ByteString    {- ^ variable name                    -} ->
  ByteString    {- ^ fallback value                   -} ->
  IO ByteString {- ^ variable value or fallback value -}
getEnvDefault name fallback = liftM (fromMaybe fallback) (getEnv name)

foreign import ccall unsafe "getenv"
   c_getenv :: CString -> IO CString

getEnvironmentPrim :: IO [ByteString]
getEnvironmentPrim = do
  c_environ <- getCEnviron
  arr <- peekArray0 nullPtr c_environ
  mapM B.packCString arr

getCEnviron :: IO (Ptr CString)
#if HAVE__NSGETENVIRON
-- You should not access @char **environ@ directly on Darwin in a bundle/shared library.
-- See #2458 and http://developer.apple.com/library/mac/#documentation/Darwin/Reference/ManPages/man7/environ.7.html
getCEnviron = nsGetEnviron >>= peek

foreign import ccall unsafe "_NSGetEnviron"
   nsGetEnviron :: IO (Ptr (Ptr CString))
#else
getCEnviron = peek c_environ_p

foreign import ccall unsafe "&environ"
   c_environ_p :: Ptr (Ptr CString)
#endif

-- |'getEnvironment' retrieves the entire environment as a
-- list of @(key,value)@ pairs.

getEnvironment :: IO [(ByteString,ByteString)] {- ^ @[(key,value)]@ -}
getEnvironment = do
  env <- getEnvironmentPrim
  return $ map (dropEq.(BC.break ((==) '='))) env
 where
   dropEq (x,y)
      | BC.head y == '=' = (x,B.tail y)
      | otherwise       = error $ "getEnvironment: insane variable " ++ BC.unpack x

-- |The 'unsetEnv' function deletes all instances of the variable name
-- from the environment.

unsetEnv :: ByteString {- ^ variable name -} -> IO ()
#if HAVE_UNSETENV
# if !UNSETENV_RETURNS_VOID
unsetEnv name = B.useAsCString name $ \ s ->
  throwErrnoIfMinus1_ "unsetenv" (c_unsetenv s)

-- POSIX.1-2001 compliant unsetenv(3)
foreign import capi unsafe "HsUnix.h unsetenv"
   c_unsetenv :: CString -> IO CInt
# else
unsetEnv name = B.useAsCString name c_unsetenv

-- pre-POSIX unsetenv(3) returning @void@
foreign import capi unsafe "HsUnix.h unsetenv"
   c_unsetenv :: CString -> IO ()
# endif
#else
unsetEnv name = putEnv (name ++ "=")
#endif

-- |'putEnv' function takes an argument of the form @name=value@
-- and is equivalent to @setEnv(key,value,True{-overwrite-})@.

putEnv :: ByteString {- ^ "key=value" -} -> IO ()
putEnv keyvalue = B.useAsCString keyvalue $ \s ->
  throwErrnoIfMinus1_ "putenv" (c_putenv s)

foreign import ccall unsafe "putenv"
   c_putenv :: CString -> IO CInt

{- |The 'setEnv' function inserts or resets the environment variable name in
     the current environment list.  If the variable @name@ does not exist in the
     list, it is inserted with the given value.  If the variable does exist,
     the argument @overwrite@ is tested; if @overwrite@ is @False@, the variable is
     not reset, otherwise it is reset to the given value.
-}

setEnv ::
  ByteString {- ^ variable name  -} ->
  ByteString {- ^ variable value -} ->
  Bool       {- ^ overwrite      -} ->
  IO ()
#ifdef HAVE_SETENV
setEnv key value ovrwrt = do
  B.useAsCString key $ \ keyP ->
    B.useAsCString value $ \ valueP ->
      throwErrnoIfMinus1_ "setenv" $
        c_setenv keyP valueP (fromIntegral (fromEnum ovrwrt))

foreign import ccall unsafe "setenv"
   c_setenv :: CString -> CString -> CInt -> IO CInt
#else
setEnv key value True = putEnv (key++"="++value)
setEnv key value False = do
  res <- getEnv key
  case res of
    Just _  -> return ()
    Nothing -> putEnv (key++"="++value)
#endif

-- | Computation 'getArgs' returns a list of the program's command
-- line arguments (not including the program name), as 'ByteString's.
--
-- Unlike 'System.Environment.getArgs', this function does no Unicode
-- decoding of the arguments; you get the exact bytes that were passed
-- to the program by the OS.  To interpret the arguments as text, some
-- Unicode decoding should be applied.
--
getArgs :: IO [ByteString]
getArgs =
  alloca $ \ p_argc ->
  alloca $ \ p_argv -> do
   getProgArgv p_argc p_argv
   p    <- fromIntegral `liftM` peek p_argc
   argv <- peek p_argv
   peekArray (p - 1) (advancePtr argv 1) >>= mapM B.packCString

foreign import ccall unsafe "getProgArgv"
  getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()
