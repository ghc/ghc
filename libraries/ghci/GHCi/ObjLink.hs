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
  , loadDLL
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
import Control.Exception (throwIO, ErrorCall(..))
import Control.Monad    ( when )
import Foreign.C
import Foreign.Marshal.Alloc ( alloca, free )
import Foreign          ( nullPtr, peek )
import GHC.Exts
import System.Posix.Internals ( CFilePath, withFilePath, peekFilePath )
import System.FilePath  ( dropExtension, normalise )

#if defined(wasm32_HOST_ARCH)
import Control.Exception (catch, evaluate)
import GHC.Wasm.Prim
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

loadDLL :: String -> IO (Either String (Ptr LoadedDLL))
loadDLL f =
  m `catch` \(err :: JSException) ->
    pure $ Left $ "loadDLL failed for " <> f <> ": " <> show err
  where
    m = do
      evaluate =<< js_loadDLL (toJSString f)
      pure $ Right nullPtr

-- See Note [Variable passing in JSFFI] for where
-- __ghc_wasm_jsffi_dyld comes from

foreign import javascript safe "__ghc_wasm_jsffi_dyld.loadDLL($1)"
  js_loadDLL :: JSString -> IO ()

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
lookupSymbolInDLL _ sym =
  throwIO $ ErrorCall $ "lookupSymbolInDLL: unsupported on wasm for " <> sym

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
initObjLinker RetainCAFs = c_initLinker_ 1
initObjLinker _ = c_initLinker_ 0

lookupSymbol :: String -> IO (Maybe (Ptr a))
lookupSymbol str_in = do
   let str = prefixUnderscore str_in
   withCAString str $ \c_str -> do
     addr <- c_lookupSymbol c_str
     if addr == nullPtr
        then return Nothing
        else return (Just addr)

lookupSymbolInDLL :: Ptr LoadedDLL -> String -> IO (Maybe (Ptr a))
lookupSymbolInDLL dll str_in = do
   let str = prefixUnderscore str_in
   withCAString str $ \c_str -> do
     addr <- c_lookupSymbolInNativeObj dll c_str
     if addr == nullPtr
       then return Nothing
       else return (Just addr)

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
loadDLL :: String -> IO (Either String (Ptr LoadedDLL))
loadDLL str0 = do
  let
     -- On Windows, addDLL takes a filename without an extension, because
     -- it tries adding both .dll and .drv.  To keep things uniform in the
     -- layers above, loadDLL always takes a filename with an extension, and
     -- we drop it here on Windows only.
     str | isWindowsHost = dropExtension str0
         | otherwise     = str0
  --
  (maybe_handle, maybe_errmsg) <- withFilePath (normalise str) $ \dll ->
    alloca $ \errmsg_ptr -> (,)
      <$> c_loadNativeObj dll errmsg_ptr
      <*> peek errmsg_ptr

  if maybe_handle == nullPtr
    then do str <- peekCString maybe_errmsg
            free maybe_errmsg
            return (Left str)
    else return (Right maybe_handle)

loadArchive :: String -> IO ()
loadArchive str = do
   withFilePath str $ \c_str -> do
     r <- c_loadArchive c_str
     when (r == 0) (throwIO (ErrorCall ("loadArchive " ++ show str ++ ": failed")))

loadObj :: String -> IO ()
loadObj str = do
   withFilePath str $ \c_str -> do
     r <- c_loadObj c_str
     when (r == 0) (throwIO (ErrorCall ("loadObj " ++ show str ++ ": failed")))

-- | @unloadObj@ drops the given dynamic library from the symbol table
-- as well as enables the library to be removed from memory during
-- a future major GC.
unloadObj :: String -> IO ()
unloadObj str =
   withFilePath str $ \c_str -> do
     r <- c_unloadObj c_str
     when (r == 0) (throwIO (ErrorCall ("unloadObj " ++ show str ++ ": failed")))

-- | @purgeObj@ drops the symbols for the dynamic library from the symbol
-- table. Unlike 'unloadObj', the library will not be dropped memory during
-- a future major GC.
purgeObj :: String -> IO ()
purgeObj str =
   withFilePath str $ \c_str -> do
     r <- c_purgeObj c_str
     when (r == 0) (throwIO (ErrorCall ("purgeObj " ++ show str ++ ": failed")))

addLibrarySearchPath :: String -> IO (Ptr ())
addLibrarySearchPath str =
   withFilePath str c_addLibrarySearchPath

removeLibrarySearchPath :: Ptr () -> IO Bool
removeLibrarySearchPath = c_removeLibrarySearchPath

findSystemLibrary :: String -> IO (Maybe String)
findSystemLibrary str = do
    result <- withFilePath str c_findSystemLibrary
    case result == nullPtr of
        True  -> return Nothing
        False -> do path <- peekFilePath result
                    free result
                    return $ Just path

resolveObjs :: IO Bool
resolveObjs = do
   r <- c_resolveObjs
   return (r /= 0)

-- ---------------------------------------------------------------------------
-- Foreign declarations to RTS entry points which does the real work;
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "loadNativeObj"           c_loadNativeObj           :: CFilePath -> Ptr CString -> IO (Ptr LoadedDLL)
foreign import ccall unsafe "lookupSymbolInNativeObj" c_lookupSymbolInNativeObj :: Ptr LoadedDLL -> CString -> IO (Ptr a)
foreign import ccall unsafe "initLinker_"             c_initLinker_             :: CInt -> IO ()
foreign import ccall unsafe "lookupSymbol"            c_lookupSymbol            :: CString -> IO (Ptr a)
foreign import ccall unsafe "loadArchive"             c_loadArchive             :: CFilePath -> IO Int
foreign import ccall unsafe "loadObj"                 c_loadObj                 :: CFilePath -> IO Int
foreign import ccall unsafe "purgeObj"                c_purgeObj                :: CFilePath -> IO Int
foreign import ccall unsafe "unloadObj"               c_unloadObj               :: CFilePath -> IO Int
foreign import ccall unsafe "resolveObjs"             c_resolveObjs             :: IO Int
foreign import ccall unsafe "addLibrarySearchPath"    c_addLibrarySearchPath    :: CFilePath -> IO (Ptr ())
foreign import ccall unsafe "findSystemLibrary"       c_findSystemLibrary       :: CFilePath -> IO CFilePath
foreign import ccall unsafe "removeLibrarySearchPath" c_removeLibrarySearchPath :: Ptr() -> IO Bool

-- -----------------------------------------------------------------------------
-- Configuration

#include "ghcautoconf.h"

cLeadingUnderscore :: Bool
#if defined(LEADING_UNDERSCORE)
cLeadingUnderscore = True
#else
cLeadingUnderscore = False
#endif

isWindowsHost :: Bool
#if defined(mingw32_HOST_OS)
isWindowsHost = True
#else
isWindowsHost = False
#endif

#endif

lookupClosure :: String -> IO (Maybe HValueRef)
lookupClosure str = do
  m <- lookupSymbol str
  case m of
    Nothing -> return Nothing
    Just (Ptr addr) -> case addrToAny# addr of
      (# a #) -> Just <$> mkRemoteRef (HValue a)
