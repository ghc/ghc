{-# OPTIONS -XRecursiveDo #-}

-- test let bindings

module Main (main) where

import Control.Monad.Fix 

t :: IO Int
t = mdo x <- return (l "1")
        let l [] = 0
            l (x:xs) = 1 + l xs
        return x

main :: IO ()
main = t >>= print
