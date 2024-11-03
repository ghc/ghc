{-# LANGUAGE Trustworthy #-}

module GHC.Internal.Data.List.NonEmpty
  ( NonEmpty(..)
  , zip
  , zipWith
  , map
  ) where

import GHC.Internal.Data.NonEmpty (NonEmpty (..), map)
import qualified GHC.Internal.Data.List as List

-- | The 'zip' function takes two streams and returns a stream of
-- corresponding pairs.
zip :: NonEmpty a -> NonEmpty b -> NonEmpty (a,b)
zip (x :| xs) (y :| ys) = (x, y) :| List.zip xs ys

-- | The 'zipWith' function generalizes 'zip'. Rather than tupling
-- the elements, the elements are combined using the function
-- passed as the first argument.
zipWith :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
zipWith f (x :| xs) (y :| ys) = f x y :| List.zipWith f xs ys
