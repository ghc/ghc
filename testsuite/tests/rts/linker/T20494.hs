{-# LANGUAGE ForeignFunctionInterface, CPP #-}
import Foreign.C.String
import Control.Monad
import System.Environment
import System.FilePath
import Foreign.Ptr
import System.Mem

-- Type of paths is different on Windows
#if defined(mingw32_HOST_OS)
type PathString = CWString
withPathString = withCWString
#else
type PathString = CString
withPathString = withCString
#endif

-- | All symbols begin with an underscore on Darwin
withUnderscore :: String -> String
#if defined(darwin_HOST_OS)
withUnderscore = ("_" ++)
#else
withUnderscore = id
#endif

foreign import ccall "initLinker"
    initLinker :: IO ()
foreign import ccall "loadObj"
    loadObj :: PathString -> IO Int
foreign import ccall "resolveObjs"
    resolveObjs :: IO Int
foreign import ccall "lookupSymbol"
    lookupSymbol :: CString -> IO (FunPtr a)
foreign import ccall "unloadObj"
    unloadObj :: PathString -> IO Int

type HelloFn = IO ()
foreign import ccall "dynamic"
  mkHello :: FunPtr HelloFn -> HelloFn

main :: IO ()
main = do
    [objPath] <- getArgs
    initLinker

    r <- withPathString objPath loadObj
    when (r /= 1) $ error "loadObj failed"

    r <- resolveObjs
    when (r /= 1) $ error "resolveObj failed"

    ptr <- withCString (withUnderscore "hello") lookupSymbol
    when (nullFunPtr == ptr) $ error "lookupSymbol failed"

    let hello = mkHello ptr
    hello

    withPathString objPath unloadObj
    when (r /= 1) $ error "unloadObj failed"

    -- Perform a major GC to ensure that the object can be unloaded.
    performMajorGC