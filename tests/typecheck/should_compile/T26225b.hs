{-# LANGUAGE DeepSubsumption #-}
{-# LANGUAGE RankNTypes #-}

module T26225b where

f :: Int -> (forall a. a->a)
f _ x = x
g :: Int -> Bool -> Bool
g _ x = x

test3 b =
  case b of
    True  -> f
    False -> g
test3' b =
  case b of
    True  -> g
    False -> f
-- Both of these currently error with:
--   * Couldn't match type: forall a. a -> a
--                    with: Bool -> Bool
