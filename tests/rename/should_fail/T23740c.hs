{-# LANGUAGE RequiredTypeArguments #-}

module T23740c where

f :: Int -> Int
f a = g a
  where
    g :: a -> a
    g = id