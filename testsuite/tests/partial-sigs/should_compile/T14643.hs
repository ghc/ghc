{-# LANGUAGE PartialTypeSignatures #-}

module T14653 where

af, ag :: (Num a,_) => a -> a
-- It's important that one signature covers both

af y = ag y
ag x = af (x-1)
