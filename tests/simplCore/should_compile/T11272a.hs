module T11272a where

import Control.Monad.Trans.State
import Control.Monad

overloaded :: Ord a => a -> a -> State () ()
overloaded x y = do
  () <- get
  when (x <= y) (overloaded y x)
{-# INLINABLE overloaded #-}
