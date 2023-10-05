{-# OPTIONS_GHC -O #-}
-- Bug only showed up with optimisation on

module Main where

import qualified Control.Exception as Exception

main :: IO ()
main = do
    unserialize
    putStrLn "all is well"

unserialize :: IO Char
unserialize =
    if definitelyTrue
        then do
            return 'a'
        else do
            Exception.evaluate (error "wrong place")

{-# NOINLINE definitelyTrue #-}
definitelyTrue :: Bool
definitelyTrue = True
