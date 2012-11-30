{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module ShouldCompile where

import Control.Monad.ST
import Data.STRef

-- (Modified now that we don't have result type signatures)

f:: forall s. ST s Int
f = do v <- newSTRef 5
       let g :: ST s Int
             -- ^ should be in scope
           g = readSTRef v
       g
