{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
module Linear7 where

incorrectLCase :: Int ⊸ Bool -> Int
incorrectLCase n = \case
  True -> n
  False -> 0
