{-# LANGUAGE UndecidableInstances #-}

module T9892 where

import Control.Applicative
import Control.Category
import Prelude hiding ((.),id)

newtype FocusingPlus w k s a = FocusingPlus { unfocusingPlus :: k (s, w) a }

instance Functor (k (s, w)) => Functor (FocusingPlus w k s) where
  fmap f (FocusingPlus as) = FocusingPlus (fmap f as)

instance Applicative (k (s, w)) => Applicative (FocusingPlus w k s) where
  pure = FocusingPlus . pure
  FocusingPlus kf <*> FocusingPlus ka = FocusingPlus (kf <*> ka)
