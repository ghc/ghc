-- Bug.hs
{-# LANGUAGE ApplicativeDo #-}
module Main where

import Data.Functor.Identity

f :: Identity () -> Identity [Int] -> Identity Int
f i0 i1 = do
    _ <- i0
    [x] <- i1
    pure (x + 42)

main :: IO ()
main = print $ f (Identity ()) (Identity [])
