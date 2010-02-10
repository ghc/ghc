{-# LANGUAGE QuasiQuotes #-}
module Main where

import Expr

main :: IO ()
main = do  print $ eval [expr|1 + 3 + 5|]
           case [expr|2|] of
             [expr|$n|] -> print n
             _           -> return ()
           case [$expr|1 + 2|] of
             [expr|$x + $y|] -> putStrLn $ show x ++ " + " ++ show y
             _                -> return ()
