module TypeOperators (
  -- * stuff
  (:-:),
  (:+:),
  Op,
  O(..),
  biO,
  Flip(..)
) where

data a :-: b

data (a :+: b) c

data a `Op` b

newtype (g `O` f) a = O { unO :: g (f a) }

biO :: (g `O` f) a
biO = undefined

newtype Flip (~>) b a = Flip { unFlip :: a ~> b }
