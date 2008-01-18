{-# LANGUAGE QuasiQuotes #-}
module Main where

import Expr

main :: IO ()
main = do  case [$expr|1 + 2|] of
             [$expr|$x + $x|] -> print x
             _                -> return ()
