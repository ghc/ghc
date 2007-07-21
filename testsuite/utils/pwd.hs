
module Main where

import System.Directory
import System.Environment

main :: IO ()
main = do args <- getArgs
          let escape = case args of
                       ["quadruple-backslash"] -> escape_quadruple_backslash
                       ["forwardslash"] -> escape_forwardslash
                       _ -> error ("pwd: Bad args: " ++ show args)
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

