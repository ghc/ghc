{-# LANGUAGE CPP, UnboxedTuples, MagicHash, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
--
--  (c) The University of Glasgow 2002-2006
--

-- ---------------------------------------------------------------------------
--      The dynamic linker for object code (.o .so .dll files)
-- ---------------------------------------------------------------------------

-- | Primarily, this module consists of an interface to the C-land
-- dynamic linker.
module GHCi.ObjLink
  ( initObjLinker, ShouldRetainCAFs(..)
  , loadDLLs
  , loadArchive
  , loadObj
  , unloadObj
  , purgeObj
  , lookupSymbol
  , lookupSymbolInDLL
  , lookupClosure
  , resolveObjs
  , addLibrarySearchPath
  , removeLibrarySearchPath
  , findSystemLibrary
  )  where

import Prelude -- See note [Why do we import Prelude here?]
import GHCi.RemoteTypes
import GHCi.Message (LoadedDLL)
import GHC.Exts

#if !defined(wasm32_HOST_ARCH)
import qualified GHCi.ObjLinker as HsLinker
#endif

#if defined(wasm32_HOST_ARCH)
import Control.Exception (catch, evaluate, throwIO, ErrorCall(..))
import Foreign          ( nullPtr )
import GHC.Wasm.Prim
#endif

#if defined(wasm32_HOST_ARCH)
import Data.List (intercalate)
#endif

-- ---------------------------------------------------------------------------
-- RTS Linker Interface
-- ---------------------------------------------------------------------------

data ShouldRetainCAFs
  = RetainCAFs
    -- ^ Retain CAFs unconditionally in linked Haskell code.
    -- Note that this prevents any code from being unloaded.
    -- It should not be necessary unless you are GHCi or
    -- hs-plugins, which needs to be able call any function
    -- in the compiled code.
  | DontRetainCAFs
    -- ^ Do not retain CAFs.  Everything reachable from foreign
    -- exports will be retained, due to the StablePtrs
    -- created by the module initialisation code.  unloadObj
    -- frees these StablePtrs, which will allow the CAFs to
    -- be GC'd and the code to be removed.

#if defined(wasm32_HOST_ARCH)

-- On wasm, retain_cafs flag is ignored, revertCAFs is a no-op
initObjLinker :: ShouldRetainCAFs -> IO ()
initObjLinker _ = pure ()

-- Batch load multiple DLLs at once via dyld to enable a single
-- dependency resolution and more parallel compilation. We pass a
-- NUL-delimited JSString to avoid array marshalling on wasm.
loadDLLs :: [String] -> IO (Either String [Ptr LoadedDLL])
loadDLLs fs =
  m `catch` \(err :: JSException) ->
    pure $ Left $ "loadDLLs failed: " <> show err
  where
    packed :: JSString
    packed = toJSString (intercalate ['\0'] fs)
    m = do
      evaluate =<< js_loadDLLs packed
      pure $ Right (replicate (length fs) nullPtr)

-- See Note [Variable passing in JSFFI] for where
-- __ghc_wasm_jsffi_dyld comes from

foreign import javascript safe "__ghc_wasm_jsffi_dyld.loadDLLs($1)"
  js_loadDLLs :: JSString -> IO ()

loadArchive :: String -> IO ()
loadArchive f = throwIO $ ErrorCall $ "loadArchive: unsupported on wasm for " <> f

loadObj :: String -> IO ()
loadObj f = throwIO $ ErrorCall $ "loadObj: unsupported on wasm for " <> f

unloadObj :: String -> IO ()
unloadObj f = throwIO $ ErrorCall $ "unloadObj: unsupported on wasm for " <> f

purgeObj :: String -> IO ()
purgeObj f = throwIO $ ErrorCall $ "purgeObj: unsupported on wasm for " <> f

lookupSymbol :: String -> IO (Maybe (Ptr a))
lookupSymbol sym = do
  r <- js_lookupSymbol $ toJSString sym
  evaluate $ if r == nullPtr then Nothing else Just r

foreign import javascript unsafe "__ghc_wasm_jsffi_dyld.lookupSymbol($1)"
  js_lookupSymbol :: JSString -> IO (Ptr a)

lookupSymbolInDLL :: Ptr LoadedDLL -> String -> IO (Maybe (Ptr a))
lookupSymbolInDLL _ _ = pure Nothing

resolveObjs :: IO Bool
resolveObjs = pure True

-- dyld does not maintain unique handles for added search paths, and
-- removeLibrarySearchPath is simply a no-op, so it's fine to return a
-- null pointer as a placeholder
addLibrarySearchPath :: String -> IO (Ptr ())
addLibrarySearchPath p = do
  evaluate =<< js_addLibrarySearchPath (toJSString p)
  pure nullPtr

foreign import javascript safe "__ghc_wasm_jsffi_dyld.addLibrarySearchPath($1)"
  js_addLibrarySearchPath :: JSString -> IO ()

removeLibrarySearchPath :: Ptr () -> IO Bool
removeLibrarySearchPath _ = pure True

findSystemLibrary :: String -> IO (Maybe String)
findSystemLibrary f = m `catch` \(_ :: JSException) -> pure Nothing
  where
    m = do
      p' <- js_findSystemLibrary (toJSString f)
      p <- evaluate $ fromJSString p'
      pure $ Just p

foreign import javascript safe "__ghc_wasm_jsffi_dyld.findSystemLibrary($1)"
  js_findSystemLibrary :: JSString -> IO JSString

#else

initObjLinker :: ShouldRetainCAFs -> IO ()
initObjLinker RetainCAFs = HsLinker.initObjLinker True
initObjLinker _ = HsLinker.initObjLinker False

lookupSymbol :: String -> IO (Maybe (Ptr a))
lookupSymbol str_in = do
   let str = prefixUnderscore str_in
   HsLinker.lookupSymbol str

lookupSymbolInDLL :: Ptr LoadedDLL -> String -> IO (Maybe (Ptr a))
lookupSymbolInDLL dll str_in = do
   let str = prefixUnderscore str_in
   HsLinker.lookupSymbolInDLL dll str

prefixUnderscore :: String -> String
prefixUnderscore
 | cLeadingUnderscore = ('_':)
 | otherwise          = id

-- | loadDLL loads a dynamic library using the OS's native linker
-- (i.e. dlopen() on Unix, LoadLibrary() on Windows).  It takes either
-- an absolute pathname to the file, or a relative filename
-- (e.g. "libfoo.so" or "foo.dll").  In the latter case, loadDLL
-- searches the standard locations for the appropriate library.
--
loadArchive :: String -> IO ()
loadArchive = HsLinker.loadArchive

loadObj :: String -> IO ()
loadObj = HsLinker.loadObj

-- | @unloadObj@ drops the given dynamic library from the symbol table
-- as well as enables the library to be removed from memory during
-- a future major GC.
unloadObj :: String -> IO ()
unloadObj = HsLinker.unloadObj

-- | @purgeObj@ drops the symbols for the dynamic library from the symbol
-- table. Unlike 'unloadObj', the library will not be dropped memory during
-- a future major GC.
purgeObj :: String -> IO ()
purgeObj = HsLinker.purgeObj

addLibrarySearchPath :: String -> IO (Ptr ())
addLibrarySearchPath = HsLinker.addLibrarySearchPath

removeLibrarySearchPath :: Ptr () -> IO Bool
removeLibrarySearchPath = HsLinker.removeLibrarySearchPath

findSystemLibrary :: String -> IO (Maybe String)
findSystemLibrary = HsLinker.findSystemLibrary

resolveObjs :: IO Bool
resolveObjs = HsLinker.resolveObjs

loadDLLs :: [String] -> IO (Either String [Ptr LoadedDLL])
loadDLLs = HsLinker.loadDLLs

-- -----------------------------------------------------------------------------
-- Configuration

#include "ghcautoconf.h"

cLeadingUnderscore :: Bool
#if defined(LEADING_UNDERSCORE)
cLeadingUnderscore = True
#else
cLeadingUnderscore = False
#endif

#endif

lookupClosure :: String -> IO (Maybe HValueRef)
lookupClosure str = do
  m <- lookupSymbol str
  case m of
    Nothing -> return Nothing
    Just (Ptr addr) -> case addrToAny# addr of
      (# a #) -> Just <$> mkRemoteRef (HValue a)
