{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
module Linear15 where

correctWhere :: Int ⊸ Int
correctWhere a = g a
  where
    f :: Int ⊸ Int
    f x = x

    g :: Int ⊸ Int
    g x = f x
