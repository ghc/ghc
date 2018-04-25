{-# LANGUAGE TypeFamilies, ExistentialQuantification #-}

module T7156 where

data T a = (a ~ ()) => T

f :: T a -> a
f T = ()
