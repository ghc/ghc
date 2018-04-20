{-# LANGUAGE PartialTypeSignatures #-}

module T14653a where

af :: (Num a,_) => a -> a
af y = ag y

ag :: (Num a,_) => a -> a
ag x = af (x-1)
