{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import System.Posix.DynamicLinker

main :: IO ()
main = do
    dl <- dlopen "./T13702" [RTLD_NOW]
    funptr <- dlsym dl "hello" :: IO (FunPtr (IO ()))
    mkAction funptr

foreign import ccall "dynamic" mkAction :: FunPtr (IO ()) -> IO ()
