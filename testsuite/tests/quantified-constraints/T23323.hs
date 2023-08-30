{-# OPTIONS_GHC -Werror -Wredundant-constraints #-}
{-# LANGUAGE QuantifiedConstraints, TypeApplications, UndecidableInstances #-}

module Lib where

import Data.Kind
import Data.Proxy

data T a
instance Show (T a) where { show x = "no" }

foo :: forall f c. (forall a. c a => Show (f a)) => Proxy c -> f Int -> Int
foo = foo

bar = foo @T @Eq
-- We don't want to report a redundant (Eq a) constraint

