{-# LANGUAGE Arrows, PatternSynonyms #-}

-- #1662

module ArrowPatSyn1 where

import Control.Arrow

data T a where
  MkT :: Show a => a -> T a

pattern P :: () => Show a => a -> T a
pattern P a = MkT a

panic :: Arrow arrow => arrow (T a) String
panic = proc (P x) ->
  show -< x
