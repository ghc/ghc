module Main where

data T = T1 Int | T2 String
  deriving (Show, Eq, Ord, Read)

data T' = T' {-# UNPACK #-} !T
  deriving (Show, Eq, Ord, Read)

t1, t2 :: T
t1 = T1 123
t2 = T2 "OK"
{-# NOINLINE t1 #-}
{-# NOINLINE t2 #-}

t'1, t'2 :: T'
t'1 = T' t1
t'2 = T' t2

main :: IO ()
main = do
  print t'1
  print t'2
