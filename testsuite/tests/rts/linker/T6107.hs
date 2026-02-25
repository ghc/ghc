{-# LANGUAGE ForeignFunctionInterface, CPP #-}
-- Test that the RTS linker merges COMMON symbols from multiple object files.
-- Without the fix, loadObj of the second .o fails with a duplicate symbol error.
import Foreign.C.String
import Control.Monad

-- Type of paths is different on Windows
#if defined(mingw32_HOST_OS)
type PathString = CWString
withPathString = withCWString
#else
type PathString = CString
withPathString = withCString
#endif

main :: IO ()
main = do
    initLinker
    r1 <- withPathString "T6107_sym1.o" loadObj
    when (r1 /= 1) $ error "loadObj T6107_sym1.o failed"
    r2 <- withPathString "T6107_sym2.o" loadObj
    when (r2 /= 1) $ error "loadObj T6107_sym2.o failed"
    r <- resolveObjs
    when (r /= 1) $ error "resolveObjs failed"
    putStrLn "OK"

foreign import ccall "initLinker" initLinker :: IO ()
foreign import ccall "loadObj"    loadObj    :: PathString -> IO Int
foreign import ccall "resolveObjs" resolveObjs :: IO Int
