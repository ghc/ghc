{-# LANGUAGE MultiParamTypeClasses #-}
module T4921 where

class C a b where
    f :: (a,b)

instance C Int Char where
    f = undefined

x = fst f

y = fst f :: Int
