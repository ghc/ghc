{-# LANGUAGE ImpredicativeTypes #-}

module T12644 where

data T a = T1 Int

instance Show (T a) where
  show (T1 x) = show x

t1 :: T a
t1 = T1 1

f :: String
f = show t1
