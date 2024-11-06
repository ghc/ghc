{-# OPTIONS_GHC -fno-spec-eval-dictfun #-}
module B (testX) where

import qualified Cls

-- this creates the big dictionary lazily
testX :: (Show a, Cls.HasConst a) => a -> Int -> IO ()
testX a b = Cls.printConst a b
