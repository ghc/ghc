{-# OPTIONS -fglasgow-exts #-}

-- !!! Scoped type variables in result signatures
module ShouldCompile where

import Control.Monad.ST
import Data.STRef

f:: ST s Int
f:: ST s Int = do
    v <- newSTRef 5
    let g :: ST s Int
             -- ^ should be in scope
        g = readSTRef v
    g
