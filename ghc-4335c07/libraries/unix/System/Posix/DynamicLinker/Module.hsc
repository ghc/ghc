#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.DynamicLinker.Module
-- Copyright   :  (c) Volker Stolz <vs@foldr.org> 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  vs@foldr.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- DLOpen support, old API
--  Derived from GModule.chs by M.Weber & M.Chakravarty which is part of c2hs
--  I left the API more or less the same, mostly the flags are different.
--
-----------------------------------------------------------------------------

module System.Posix.DynamicLinker.Module (

--  Usage:
--  ******
--
--  Let's assume you want to open a local shared library 'foo' (./libfoo.so)
--  offering a function
--    char * mogrify (char*,int)
--  and invoke str = mogrify("test",1):
--
--  type Fun = CString -> Int -> IO CString
--  foreign import dynamic unsafe fun__ :: FunPtr Fun -> Fun
--
--  withModule (Just ".") ("libfoo.so") [RTLD_NOW] $ \ mod -> do
--     funptr <- moduleSymbol mod "mogrify"
--     let fun = fun__ funptr
--     withCString "test" $ \ str -> do
--       strptr <- fun str 1
--       strstr <- peekCString strptr
--       ...

      Module
    , moduleOpen             -- :: String -> ModuleFlags -> IO Module
    , moduleSymbol           -- :: Source -> String -> IO (FunPtr a)
    , moduleClose            -- :: Module -> IO Bool
    , moduleError            -- :: IO String
    , withModule             -- :: Maybe String
                             -- -> String
                             -- -> [ModuleFlags ]
                             -- -> (Module -> IO a)
                             -- -> IO a
    , withModule_            -- :: Maybe String
                             -- -> String
                             -- -> [ModuleFlags]
                             -- -> (Module -> IO a)
                             -- -> IO ()
    )
where

#include "HsUnix.h"

import System.Posix.DynamicLinker
import System.Posix.DynamicLinker.Common
import Foreign.Ptr      ( Ptr, nullPtr, FunPtr )
import System.Posix.Internals ( withFilePath )

unModule              :: Module -> (Ptr ())
unModule (Module adr)  = adr

-- Opens a module (EXPORTED)
--

moduleOpen :: String -> [RTLDFlags] -> IO Module
moduleOpen file flags = do
  modPtr <- withFilePath file $ \ modAddr -> c_dlopen modAddr (packRTLDFlags flags)
  if (modPtr == nullPtr)
      then moduleError >>= \ err -> ioError (userError ("dlopen: " ++ err))
      else return $ Module modPtr

-- Gets a symbol pointer from a module (EXPORTED)
--
moduleSymbol :: Module -> String -> IO (FunPtr a)
moduleSymbol file sym = dlsym (DLHandle (unModule file)) sym

-- Closes a module (EXPORTED)
--
moduleClose     :: Module -> IO ()
moduleClose file  = dlclose (DLHandle (unModule file))

-- Gets a string describing the last module error (EXPORTED)
--
moduleError :: IO String
moduleError  = dlerror


-- Convenience function, cares for module open- & closing
-- additionally returns status of `moduleClose' (EXPORTED)
--
withModule :: Maybe String
           -> String
           -> [RTLDFlags]
           -> (Module -> IO a)
           -> IO a
withModule mdir file flags p = do
  let modPath = case mdir of
                  Nothing -> file
                  Just dir  -> dir ++ if ((head (reverse dir)) == '/')
                                       then file
                                       else ('/':file)
  modu <- moduleOpen modPath flags
  result <- p modu
  moduleClose modu
  return result

withModule_ :: Maybe String
            -> String
            -> [RTLDFlags]
            -> (Module -> IO a)
            -> IO ()
withModule_ dir file flags p = withModule dir file flags p >>= \ _ -> return ()
