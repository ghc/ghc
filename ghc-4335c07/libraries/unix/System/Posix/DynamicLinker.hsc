#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.DynamicLinker
-- Copyright   :  (c) Volker Stolz <vs@foldr.org> 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  vs@foldr.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- Dynamic linker support through dlopen()
-----------------------------------------------------------------------------

module System.Posix.DynamicLinker (

    module System.Posix.DynamicLinker.Prim,
    dlopen,
    dlsym,
    dlerror,
    dlclose,
    withDL, withDL_,
    undl,
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

import System.Posix.DynamicLinker.Common
import System.Posix.DynamicLinker.Prim

#include "HsUnix.h"

import Control.Exception        ( bracket )
import Control.Monad    ( liftM )
import Foreign
import System.Posix.Internals ( withFilePath )

dlopen :: FilePath -> [RTLDFlags] -> IO DL
dlopen path flags = do
  withFilePath path $ \ p -> do
    liftM DLHandle $ throwDLErrorIf "dlopen" (== nullPtr) $ c_dlopen p (packRTLDFlags flags)

withDL :: String -> [RTLDFlags] -> (DL -> IO a) -> IO a
withDL file flags f = bracket (dlopen file flags) (dlclose) f

withDL_ :: String -> [RTLDFlags] -> (DL -> IO a) -> IO ()
withDL_ file flags f = withDL file flags f >> return ()
