{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

newtype MyMaybe a = MyMaybe (Maybe a)
  deriving (Functor, Show)

main :: IO ()
main = print $ fmap (+1) $ MyMaybe $ Just (10 :: Int)
