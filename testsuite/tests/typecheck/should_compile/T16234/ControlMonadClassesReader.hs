{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module ControlMonadClassesReader where

import qualified Control.Monad.Trans.State.Lazy as SL
import ControlMonadClassesCore
import ControlMonadClassesEffects
import Control.Monad.Trans.Class
import Data.Kind (Type)
import DataPeano

class Monad m => MonadReaderN (n :: Peano) (r :: Type) m
instance Monad m => MonadReaderN 'Zero r (SL.StateT r m)
instance (MonadTrans t, Monad (t m), MonadReaderN n r m, Monad m)
  => MonadReaderN ('Succ n) r (t m)

type MonadReader e m = MonadReaderN (Find (EffReader e) m) e m
