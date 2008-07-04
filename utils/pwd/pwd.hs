
module Main where

import System.Directory
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
    args <- getArgs
    escape <- case args of
              ["quadruple-backslash"] -> return escape_quadruple_backslash
              ["forwardslash"] ->        return escape_forwardslash
              _ -> do hPutStrLn stderr ("Bad args: " ++ show args)
                      hPutStrLn stderr
                                "Usage: pwd {forwardslash|quadruple-backslash}"
                      exitFailure
    d <- getCurrentDirectory
    putStr $ concatMap escape d

-- In prog006 we have to escape \ twice, once to get through sed and
-- again to get through parsing pkg.conf
escape_quadruple_backslash :: Char -> String
escape_quadruple_backslash '\\' = "\\\\\\\\"
escape_quadruple_backslash c = [c]

-- Normally we can get away with just replacing backslashes with forwardslashes
escape_forwardslash :: Char -> String
escape_forwardslash '\\' = "/"
escape_forwardslash c = [c]

