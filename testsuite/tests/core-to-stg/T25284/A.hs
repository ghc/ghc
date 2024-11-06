{-# OPTIONS_GHC -fspec-eval-dictfun #-}
module A (testX) where

import qualified Cls

-- this creates the big dictionary strictly because of speculative evaluation
testX :: (Show a, Cls.HasConst a) => a -> Int -> IO ()
testX a b = Cls.printConst a b
