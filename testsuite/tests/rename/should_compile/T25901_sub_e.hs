{-# LANGUAGE ExplicitNamespaces #-}

module T25901_sub_e where

import Control.Applicative qualified as A (type Applicative (data ..))
import Data.Either qualified as E (type Either (data ..))

f :: (a -> b) -> E.Either e a -> E.Either e b
f g b = E.Right g A.<*> b