{-# LANGUAGE LambdaCase #-}

module TcLambdaCase where

import Data.Bits ((.|.))

f1 :: (a -> a) -> (a -> a)
f1 = \case x -> x

f2 :: Num a => a -> a
f2 = \case x -> x + x

f3 :: Int -> (Int, Int)
f3 = (\case y -> (y + y, y * y)) . (.|. 12)

f4 = \case _ -> undefined

