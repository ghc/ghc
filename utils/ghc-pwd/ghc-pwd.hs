
module Main where

import System.Directory
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do d <- getCurrentDirectory
                 putStr $ map forwardifySlashes d
        _ -> do hPutStrLn stderr ("Bad args: " ++ show args)
                hPutStrLn stderr "Usage: ghc-pwd"
                exitFailure

forwardifySlashes :: Char -> Char
forwardifySlashes '\\' = '/'
forwardifySlashes c = c

