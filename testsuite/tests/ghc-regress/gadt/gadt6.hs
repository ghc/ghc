{-# OPTIONS -fglasgow-exts #-}

module ShouldCompile where

data T a where
  T :: b -> (b->Int) -> a -> T a

f (T b f a) = a

