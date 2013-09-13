{-# LANGUAGE ForeignFunctionInterface, CPP #-}
import Foreign.C.String
import Control.Monad
import System.Environment

#if defined(mingw32_HOST_OS)
type PathString = CWString
withPathString = withCWString
#else
type PathString = CString
withPathString = withCString
#endif

main = do
    [object] <- getArgs
    initLinker
    r <- withPathString object $ \s -> loadObj s
    when (r /= 1) $ error "loadObj failed"
    r <- resolveObjs
    when (r /= 1) $ error "resolveObj failed"
    putStrLn "success"

foreign import ccall "initLinker" initLinker :: IO ()
foreign import ccall "loadObj" loadObj :: PathString -> IO Int
foreign import ccall "resolveObjs" resolveObjs :: IO Int
