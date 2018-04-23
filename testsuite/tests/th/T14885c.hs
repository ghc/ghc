{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module T14885c where

pattern P1 :: forall a. a -> Maybe a
pattern P1 x <- Just x where
  P1 x = Just (x :: a)

$([d| pattern P2 :: forall a. a -> Maybe a
      pattern P2 x <- Just x where
        P2 x = Just (x :: a)
    |])
