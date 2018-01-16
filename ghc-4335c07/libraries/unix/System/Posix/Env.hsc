{-# LANGUAGE CApiFFI #-}
#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Env
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

module System.Posix.Env (
      getEnv
    , getEnvDefault
    , getEnvironmentPrim
    , getEnvironment
    , setEnvironment
    , putEnv
    , setEnv
    , unsetEnv
    , clearEnv
) where

#include "HsUnix.h"

import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Control.Monad
import Data.Maybe (fromMaybe)
import System.Posix.Internals

#if !MIN_VERSION_base(4,7,0)
-- needed for backported local 'newFilePath' binding in 'putEnv'
import GHC.IO.Encoding (getFileSystemEncoding)
import qualified GHC.Foreign as GHC (newCString)
#endif

-- |'getEnv' looks up a variable in the environment.

getEnv ::
  String            {- ^ variable name  -} ->
  IO (Maybe String) {- ^ variable value -}
getEnv name = do
  litstring <- withFilePath name c_getenv
  if litstring /= nullPtr
     then liftM Just $ peekFilePath litstring
     else return Nothing

-- |'getEnvDefault' is a wrapper around 'getEnv' where the
-- programmer can specify a fallback if the variable is not found
-- in the environment.

getEnvDefault ::
  String    {- ^ variable name                    -} ->
  String    {- ^ fallback value                   -} ->
  IO String {- ^ variable value or fallback value -}
getEnvDefault name fallback = liftM (fromMaybe fallback) (getEnv name)

foreign import ccall unsafe "getenv"
   c_getenv :: CString -> IO CString

getEnvironmentPrim :: IO [String]
getEnvironmentPrim = do
  c_environ <- getCEnviron
  -- environ can be NULL
  if c_environ == nullPtr
    then return []
    else do
      arr <- peekArray0 nullPtr c_environ
      mapM peekFilePath arr

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

getEnvironment :: IO [(String,String)] {- ^ @[(key,value)]@ -}
getEnvironment = do
  env <- getEnvironmentPrim
  return $ map (dropEq.(break ((==) '='))) env
 where
   dropEq (x,'=':ys) = (x,ys)
   dropEq (x,_)      = error $ "getEnvironment: insane variable " ++ x

-- |'setEnvironment' resets the entire environment to the given list of
-- @(key,value)@ pairs.

setEnvironment ::
  [(String,String)] {- ^ @[(key,value)]@ -} ->
  IO ()
setEnvironment env = do
  clearEnv
  forM_ env $ \(key,value) ->
    setEnv key value True {-overwrite-}

-- |The 'unsetEnv' function deletes all instances of the variable name
-- from the environment.

unsetEnv :: String {- ^ variable name -} -> IO ()
#if HAVE_UNSETENV
# if !UNSETENV_RETURNS_VOID
unsetEnv name = withFilePath name $ \ s ->
  throwErrnoIfMinus1_ "unsetenv" (c_unsetenv s)

-- POSIX.1-2001 compliant unsetenv(3)
foreign import capi unsafe "HsUnix.h unsetenv"
   c_unsetenv :: CString -> IO CInt
# else
unsetEnv name = withFilePath name c_unsetenv

-- pre-POSIX unsetenv(3) returning @void@
foreign import capi unsafe "HsUnix.h unsetenv"
   c_unsetenv :: CString -> IO ()
# endif
#else
unsetEnv name = putEnv (name ++ "=")
#endif

-- |'putEnv' function takes an argument of the form @name=value@
-- and is equivalent to @setEnv(key,value,True{-overwrite-})@.

putEnv :: String {- ^ "key=value" -} -> IO ()
putEnv keyvalue = do s <- newFilePath keyvalue
                     -- Do not free `s` after calling putenv.
                     -- According to SUSv2, the string passed to putenv
                     -- becomes part of the environment. #7342
                     throwErrnoIfMinus1_ "putenv" (c_putenv s)
#if !MIN_VERSION_base(4,7,0)
    where
      newFilePath :: FilePath -> IO CString
      newFilePath fp = getFileSystemEncoding >>= \enc -> GHC.newCString enc fp
#endif

foreign import ccall unsafe "putenv"
   c_putenv :: CString -> IO CInt

{- |The 'setEnv' function inserts or resets the environment variable name in
     the current environment list.  If the variable @name@ does not exist in the
     list, it is inserted with the given value.  If the variable does exist,
     the argument @overwrite@ is tested; if @overwrite@ is @False@, the variable is
     not reset, otherwise it is reset to the given value.
-}

setEnv ::
  String {- ^ variable name  -} ->
  String {- ^ variable value -} ->
  Bool   {- ^ overwrite      -} ->
  IO ()
#ifdef HAVE_SETENV
setEnv key value ovrwrt = do
  withFilePath key $ \ keyP ->
    withFilePath value $ \ valueP ->
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

-- |The 'clearEnv' function clears the environment of all name-value pairs.
clearEnv :: IO ()
#if HAVE_CLEARENV
clearEnv = void c_clearenv

foreign import ccall unsafe "clearenv"
  c_clearenv :: IO Int
#else
-- Fallback to 'environ[0] = NULL'.
clearEnv = do
  c_environ <- getCEnviron
  unless (c_environ == nullPtr) $
    poke c_environ nullPtr
#endif
