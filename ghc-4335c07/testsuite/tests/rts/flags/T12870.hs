module T12870 where

import System.Environment

main :: IO ()
main = getArgs >>= putStr . show
