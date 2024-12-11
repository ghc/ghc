{-# LANGUAGE ScopedTypeVariables #-}

module T25437 where

class C a where
  foo :: Int -> Maybe a

instance C (Maybe x) where
  foo :: Int -> Maybe [a]

instance C [x] where
  foo :: forall b. Int -> Maybe [b]
  foo _ = Just @[b] []

  something :: x -> x
  something = ()
