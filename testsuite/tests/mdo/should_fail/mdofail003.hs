{-# OPTIONS -XRecursiveDo #-}

-- shadowing is not allowed II

module Main (main) where

import Control.Monad.Fix 

t :: IO ()
t = mdo x <- return 1
        let x 0 = 4
        return ()

main :: IO ()
main = t 
