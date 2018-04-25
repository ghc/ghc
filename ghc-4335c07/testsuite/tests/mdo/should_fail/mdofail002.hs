{-# OPTIONS -XRecursiveDo #-}

-- shadowing is not allowed

module Main (main) where

import Control.Monad.Fix 

t :: IO ()
t = mdo x <- return 1
        x <- return 2
        return ()

main :: IO ()
main = t 
