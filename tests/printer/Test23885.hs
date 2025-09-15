{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UnicodeSyntax #-}
module Test23885 where

import Control.Monad (Monad(..), join, ap)
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))

class Monoidy to comp id m | m to → comp id where
  munit :: id `to` m
  mjoin :: (m `comp` m) `to` m

newtype Sum a = Sum a deriving Show
instance Num a ⇒ Monoidy (→) (,) () (Sum a) where
  munit _ = Sum 0
  mjoin (Sum x, Sum y) = Sum $ x + y

data NT f g = NT { runNT :: ∀ α. f α → g α }
