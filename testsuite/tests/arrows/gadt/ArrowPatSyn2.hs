{-# LANGUAGE Arrows, PatternSynonyms #-}

-- #1662

module ArrowPatSyn2 where

import Control.Arrow

data T where
  MkT :: a -> T

pattern P :: () => forall a. () => a -> T
pattern P a = MkT a

panic :: Arrow arrow => arrow T T
panic = proc (P x) ->
  MkT -< x
