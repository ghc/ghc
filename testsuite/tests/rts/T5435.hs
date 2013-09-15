{-# LANGUAGE ForeignFunctionInterface, CPP #-}
import Foreign.C.String
import Control.Monad
import System.Environment
import Foreign.Ptr

#if defined(mingw32_HOST_OS)
type PathString = CWString
withPathString = withCWString
#else
type PathString = CString
withPathString = withCString
#endif

main = do
    [ty, object] <- getArgs
    initLinker
    if ty == "dyn"
      then do
        r <- withPathString object $ \s -> addDLL s
        when (r /= nullPtr) $ error =<< peekCString r
      else do
        r <- withPathString object $ \s -> loadObj s
        when (r /= 1) $ error "loadObj failed"
    r <- resolveObjs
    when (r /= 1) $ error "resolveObj failed"
    putStrLn "success"

foreign import ccall "initLinker" initLinker :: IO ()
foreign import ccall "addDLL" addDLL :: PathString -> IO CString
foreign import ccall "loadObj" loadObj :: PathString -> IO Int
foreign import ccall "resolveObjs" resolveObjs :: IO Int
