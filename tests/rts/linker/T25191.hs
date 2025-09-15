{-# LANGUAGE ForeignFunctionInterface, CPP #-}
import Foreign.C.String
import Control.Monad
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

main = do
    initLinker
    r1 <- withPathString "foo1.o" loadObj
    when (r1 /= 1) $ error "loadObj failed"
    r2 <- withPathString "foo2.o" loadObj
    when (r2 /= 1) $ error "loadObj failed"
    r <- resolveObjs
    when (r /= 1) $ error "resolveObj failed"
    putStrLn "success"

foreign import ccall "initLinker" initLinker :: IO ()
foreign import ccall "addDLL" addDLL :: PathString -> IO CString
foreign import ccall "loadObj" loadObj :: PathString -> IO Int
foreign import ccall "resolveObjs" resolveObjs :: IO Int
