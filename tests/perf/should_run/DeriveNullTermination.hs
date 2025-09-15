{-# LANGUAGE DeriveFoldable #-}

module Main where

-- Trying to check if this is null from left to right or right to left
-- will produce an infinite loop.
data Ouch a = Ouch (Ouch a) a (Ouch a) deriving Foldable

ouch :: a -> Ouch a
ouch a = v where v = Ouch v a v

newtype Tuplouch a = Tuplouch (Ouch (a, Int)) deriving Foldable

main :: IO ()
main = do
  print $ null (ouch ())
  print $ null (Tuplouch (ouch ((), 3)))
