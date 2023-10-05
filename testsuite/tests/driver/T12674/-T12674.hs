{-# LANGUAGE ForeignFunctionInterface, CPP #-}
import Foreign.C
foreign import ccall unsafe "test" test :: CInt -> IO ()

main :: IO ()
-- Use conditional language to test passing a file with a filename
-- starting with a hyphen to the preprocessor.
#if defined(__GLASGOW_HASKELL__)
main = test 3
#endif
