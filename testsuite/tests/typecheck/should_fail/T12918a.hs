{-# LANGUAGE DefaultSignatures #-}
module T12918a where

import Control.Monad.Trans.Class

class Monad m => MonadSupply m where
  fresh :: m Integer
  default fresh :: MonadTrans t => t m Integer
  fresh = lift fresh
