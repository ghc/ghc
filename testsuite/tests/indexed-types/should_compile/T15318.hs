{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module T15138 where

data family Sn a
newtype instance Sn (Either a b) where
  SnC :: forall b a. Char -> Sn (Either a b)
