{-# LANGUAGE LambdaCase #-}
module Test24748 where

instance SDecide Nat where
  SZero %~ (SSucc _) = Disproved (\case)

foo = (\case)
bar = (\cases)
