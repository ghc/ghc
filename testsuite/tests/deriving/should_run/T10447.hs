{-# LANGUAGE DeriveFoldable, GADTs, StandaloneDeriving #-}
module Main where

class (a ~ Int) => Foo a
instance Foo Int

data A a where
  A1 :: Ord a            => a        -> A a
  A2 ::                     Int      -> A Int
  A3 :: b ~ Int          => b        -> A Int
  A4 :: a ~ Int          => Int      -> A a
  A5 :: a ~ Int          => a        -> A a
  A6 :: (a ~ b, b ~ Int) => Int -> b -> A a
  A7 :: Foo a            => Int -> a -> A a

deriving instance Foldable A

data HK f a where
  HK1 :: f a -> HK f (f a)
  HK2 :: f a -> HK f a

deriving instance Foldable f => Foldable (HK f)

one :: Int
one = 1

main :: IO ()
main = do
  mapM_ (print . foldr (+) one)
    [ A1 one
    , A2 one
    , A3 one
    , A4 one
    , A5 one
    , A6 one one
    , A7 one one
    ]
  mapM_ (print . foldr mappend Nothing)
    [ HK1 (Just "Hello")
    , HK2 (Just (Just "World"))
    ]
