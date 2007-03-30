
module Main where

import System.Directory

main :: IO ()
main = do d <- getCurrentDirectory
          putStr $ concatMap escape d

-- We have to escape \ twice, once to get through sed and again to get
-- through parsing pkg.conf
escape :: Char -> String
escape '\\' = "\\\\\\\\"
escape c = [c]

