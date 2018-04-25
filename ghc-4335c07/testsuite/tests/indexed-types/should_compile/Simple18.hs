{-# LANGUAGE TypeFamilies #-}

module Simple18 where

type family F a

type instance F Int = [Int]

foo :: F Int
foo = [1]