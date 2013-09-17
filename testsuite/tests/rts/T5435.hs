{-# LANGUAGE ForeignFunctionInterface, CPP #-}
import Foreign.C.String
import Control.Monad
import System.Environment
import System.FilePath
import Foreign.Ptr

-- Type of paths is different on Windows
#if defined(mingw32_HOST_OS)
type PathString = CWString
withPathString = withCWString
#else
type PathString = CString
withPathString = withCString
#endif

mungeDLL :: FilePath -> FilePath
#if defined(mingw32_HOST_OS) || defined(cygwin32_HOST_OS)
-- Strip extension because addDLL on Windows doesn't want it
mungeDLL f =
    let (base, ext) = splitExtension f
    in if ext == ".dll" then base else error ("unexpected DLL file name: " ++ f)
#else
mungeDLL = id
#endif

main = do
    [ty, object] <- getArgs
    initLinker
    if ty == "dyn"
      then do
        r <- withPathString (mungeDLL object) $ \s -> addDLL s
        when (r /= nullPtr) $ error =<< peekCString r
      else do
        r <- withPathString object $ \s -> loadObj s
        when (r /= 1) $ error "loadObj failed"
    r <- resolveObjs
    when (r /= 1) $ error "resolveObj failed"
    putStrLn "success"

{-
    f <- withCString (mungeSymbol "do_checks") lookupSymbol
    when (f == nullFunPtr) $ error "lookupSymbol failed"
    mkIO f

foreign import ccall "lookupSymbol" lookupSymbol :: CString -> IO (FunPtr (IO ()))
foreign import ccall "dynamic" mkIO :: FunPtr (IO ()) -> IO ()

mungeSymbol :: String -> String
#if LEADING_UNDERSCORE
mungeSymbol s = "_" ++ s -- Mac OS X
#else
mungeSymbol = id
#endif
-}

foreign import ccall "initLinker" initLinker :: IO ()
foreign import ccall "addDLL" addDLL :: PathString -> IO CString
foreign import ccall "loadObj" loadObj :: PathString -> IO Int
foreign import ccall "resolveObjs" resolveObjs :: IO Int
