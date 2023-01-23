{-# LANGUAGE NoEmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}
module RnEmptyCaseFail where

f = case () of

g = \case

h = \cases

j = proc x -> do \case

k = proc x -> do case () of
