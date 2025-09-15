{-# OPTIONS -XRecursiveDo #-}

-- let bindings are monomorphic if used prior to their definition

module Main (main) where

import Control.Monad.Fix 

t :: IO (Int, Int)
t = mdo x <- return (l "1", l [1,2,3])
        let l [] = 0
            l (x:xs) = 1 + l xs
        return x

main :: IO ()
main = t >>= print
