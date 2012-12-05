{-# LANGUAGE Rank2Types #-}

module T6069 where

import Control.Monad.ST
import Data.STRef

fourty_two :: forall s. ST s Int
fourty_two = do
   x <- newSTRef (42::Int)
   readSTRef x

f1 = (print . runST) fourty_two -- (1)
f2 = (print . runST) $ fourty_two -- (2)
f3 = ((print . runST) $) fourty_two -- (3)


