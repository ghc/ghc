module Main (main) where

import Distribution.Simple

main :: IO ()
main = defaultMainWithHooks autoconfUserHooks
