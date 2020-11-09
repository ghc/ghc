{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import System.Exit
import System.Timeout

foreign import ccall safe "ghc_debug.h pauseAndUseRtsAPIAndResume"
    pauseAndUseRtsAPIAndResume
        :: (StablePtr (Int -> Int))
        -> Int
        -> Int
        -> Int
        -> (StablePtr (IO Int))
        -> IO ()

main :: IO ()
main = do
    addOne <- newStablePtr ((+1) :: Int -> Int)
    ioOne <- newStablePtr (return 1 :: IO Int)
    successMay <- timeout 5000000 $ pauseAndUseRtsAPIAndResume
        addOne
        1
        2
        3
        ioOne
    case successMay of
        Nothing -> exitFailure
        Just () -> exitSuccess
