{-# LANGUAGE RequiredTypeArguments #-}
module Main where

data A where
   A :: forall a -> A

main :: IO ()
main = do
  let a = A Int
  let A b = a
  pure ()

