#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.DynamicLinker.Common
-- Copyright   :  (c) Volker Stolz <vs@foldr.org> 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  vs@foldr.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- Dynamic linker support through dlopen()
-----------------------------------------------------------------------------

module System.Posix.DynamicLinker.Common (

    module System.Posix.DynamicLinker.Prim,
    dlsym,
    dlerror,
    dlclose,
    undl,
    throwDLErrorIf,
    Module(..)
    )

--  Usage:
--  ******
--
--  Let's assume you want to open a local shared library \'foo\' (.\/libfoo.so)
--  offering a function
--    @char \* mogrify (char\*,int)@
--  and invoke @str = mogrify("test",1)@:
--
--
--  type Fun = CString -> Int -> IO CString
--  foreign import dynamic unsafe fun__ :: FunPtr Fun -> Fun
--
--  withDL "libfoo.so" [RTLD_NOW] \$ \\ mod -> do
--     funptr <- dlsym mod "mogrify"
--     let fun = fun__ funptr
--     withCString "test" \$ \\ str -> do
--       strptr <- fun str 1
--       strstr <- peekCString strptr
--       ...
--

where

#include "HsUnix.h"

import System.Posix.DynamicLinker.Prim
import Foreign
import Foreign.C

dlclose :: DL -> IO ()
dlclose (DLHandle h) = throwDLErrorIf_ "dlclose" (/= 0) $ c_dlclose h
dlclose h = error $ "dlclose: invalid argument" ++ (show h)

dlerror :: IO String
dlerror = c_dlerror >>= peekCString

-- |'dlsym' returns the address binding of the symbol described in @symbol@,
-- as it occurs in the shared object identified by @source@.

dlsym :: DL -> String -> IO (FunPtr a)
dlsym source symbol = do
  withCAString symbol $ \ s -> do
    throwDLErrorIf "dlsym" (== nullFunPtr) $ c_dlsym (packDL source) s

-- |'undl' obtains the raw handle. You mustn't do something like
-- @withDL mod flags $ liftM undl >>= \ p -> use p@

undl :: DL -> Ptr ()
undl = packDL

throwDLErrorIf :: String -> (a -> Bool) -> IO a -> IO a
throwDLErrorIf s p f = do
  r <- f
  if (p r)
    then dlerror >>= \ err -> ioError (userError ( s ++ ": " ++ err))
    else return r

throwDLErrorIf_ :: String -> (a -> Bool) -> IO a -> IO ()
throwDLErrorIf_ s p f = throwDLErrorIf s p f >> return ()

-- abstract handle for dynamically loaded module (EXPORTED)
--
newtype Module = Module (Ptr ())
